#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, json, re
from typing import List, Tuple, Dict, Optional, Set

RULE_LOGS = False

_INEQ_RE = re.compile(r"^\(?\s*([A-Za-z_][\w\[\]\.:]*)\s*(<=|<|>=|>)\s*(-?\d+)\s*\)?$")

def _parse_ineq_atom(atom: str):
    s = atom.strip()
    if s.startswith('(') and s.endswith(')'):
        s = s[1:-1].strip()
    m = _INEQ_RE.match(s)
    if not m:
        return None
    sig, op, val = m.group(1), m.group(2), int(m.group(3))
    if op == "<=":
        return (sig, "le", val)
    elif op == ">=":
        return (sig, "ge", val)
    elif op == "<":
        return (sig, "le", val - 1)
    elif op == ">":
        return (sig, "ge", val + 1)
    return None

def sanitize_spaces(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip())

def parse_one_sva(line: str) -> Optional[Tuple[str, str, str]]:
    m = re.match(r'\s*assert\s+property\s*\(\s*(.*?)\s*\)\s*;\s*$', line, re.IGNORECASE)
    if not m:
        return None
    content = m.group(1)
    clock = ""
    if content.startswith('@'):
        paren_count = 0
        in_clock = False
        clock_end = 0
        for i, ch in enumerate(content):
            if ch == '@':
                in_clock = True
            elif in_clock:
                if ch == '(':
                    paren_count += 1
                elif ch == ')':
                    paren_count -= 1
                    if paren_count == 0:
                        clock_end = i + 1
                        break
        if clock_end > 0:
            clock = content[:clock_end].strip()
            content = content[clock_end:].strip()
    parts = content.split('|->')
    if len(parts) != 2:
        return None
    pre = parts[0].strip()
    post = parts[1].strip()
    return clock, pre, post

class Node:
    __slots__=("op","kids","atom")
    def __init__(self,op:str,kids:Optional[List['Node']]=None,atom:Optional[str]=None):
        self.op=op; self.kids=kids or []; self.atom=atom
    def __repr__(self):
        if self.op=="ATOM": return f"ATOM({self.atom})"
        if self.op in ("TRUE","FALSE"): return self.op
        if self.op=="NOT": return f"NOT({self.kids[0]!r})"
        return f"{self.op}({', '.join(repr(k) for k in self.kids)})"

def make_atom(s:str)->Node:
    s=sanitize_spaces(s)
    if s.lower()=="true": return Node("TRUE")
    if s.lower()=="false": return Node("FALSE")
    return Node("ATOM",atom=s)

def strip_outer_parens(t:str)->str:
    t=t.strip()
    while t.startswith('(') and t.endswith(')'):
        d=0; ok=True
        for i,ch in enumerate(t):
            if ch=='(' : d+=1
            elif ch==')':
                d-=1
                if d==0 and i!=len(t)-1: ok=False; break
        if ok: t=t[1:-1].strip()
        else: break
    return t

def split_top_level(t:str, ch:str)->List[str]:
    parts=[]; d=0; last=0; i=0
    while i<len(t):
        c=t[i]
        if c=='(' : d+=1
        elif c==')': d-=1
        elif d==0 and c==ch: parts.append(t[last:i].strip()); last=i+1
        i+=1
    parts.append(t[last:].strip())
    return [p for p in parts if p!=""]

def parse_boolean_expr(text:str)->Node:
    t=sanitize_spaces(text).replace("&&","&").replace("||","|")
    if "##" in t:
        return make_atom(t)
    t=strip_outer_parens(t)
    while t.startswith('!'):
        return Node("NOT",[parse_boolean_expr(t[1:].strip())])
    ors=split_top_level(t,'|')
    if len(ors)>1: return Node("OR",[parse_boolean_expr(p) for p in ors])
    ands=split_top_level(t,'&')
    if len(ands)>1: return Node("AND",[parse_boolean_expr(p) for p in ands])
    return make_atom(t)

def to_nnf(n:Node)->Node:
    if n.op=="NOT":
        x=n.kids[0]
        if x.op=="NOT":  return to_nnf(x.kids[0])
        if x.op=="AND":  return Node("OR" ,[to_nnf(Node("NOT",[c])) for c in x.kids])
        if x.op=="OR":   return Node("AND",[to_nnf(Node("NOT",[c])) for c in x.kids])
        if x.op=="TRUE": return Node("FALSE")
        if x.op=="FALSE":return Node("TRUE")
        return Node("NOT",[to_nnf(x)])
    if n.op in ("AND","OR"): return Node(n.op,[to_nnf(c) for c in n.kids])
    return n

def canonical_key(n:Node)->str:
    if n.op=="ATOM": return f"A:{n.atom}"
    if n.op=="TRUE": return "T"
    if n.op=="FALSE":return "F"
    if n.op=="NOT":  return f"N:{canonical_key(n.kids[0])}"
    ks=sorted(canonical_key(c) for c in n.kids)
    return f"{n.op}(" + ",".join(ks) + ")"

def flatten_assoc(n:Node)->Node:
    if n.op not in ("AND","OR"): return n
    out=[]
    for c in n.kids:
        if c.op==n.op: out.extend(c.kids)
        else: out.append(c)
    return Node(n.op,out)

def remove_dups_and_sort(n:Node)->Node:
    if n.op not in ("AND","OR"): return n
    uniq={canonical_key(c):c for c in n.kids}
    return Node(n.op,[uniq[k] for k in sorted(uniq)])

def contains_complement_pair(kset:Dict[str,Node])->bool:
    atoms=set(); negs=set()
    for _,c in kset.items():
        if c.op=="ATOM": atoms.add(canonical_key(c))
        elif c.op=="NOT" and c.kids[0].op=="ATOM": negs.add(canonical_key(c.kids[0]))
    return any(a in negs for a in atoms)

_RANGE_RE = re.compile(r"\#\#\s*\[\s*(\d+)\s*:\s*(\d+)\s*\]")

def _find_top_level_seqop(t:str):
    t=sanitize_spaces(t); d=0; i=0
    while i<len(t)-1:
        c=t[i]
        if c=='(' : d+=1
        elif c==')': d-=1
        elif c=='#' and i+1<len(t) and t[i+1]=='#' and d==0:
            m=_RANGE_RE.match(t[i:])
            if m and m.start()==0:
                mval=int(m.group(1)); nval=int(m.group(2))
                lhs=t[:i].strip(); rhs=t[i+m.end():].strip()
                return ('range',(mval,nval),lhs,rhs)
            j=i+2; m2=re.match(r"\s*(\d+)\s*",t[j:])
            if m2:
                kval=int(m2.group(1)); lhs=t[:i].strip(); rhs=t[j+m2.end():].strip()
                return ('fix',kval,lhs,rhs)
            return None
        i+=1
    return None

def _seqrepr(s:str):
    s=strip_outer_parens(s); r=_find_top_level_seqop(s)
    if not r: return None
    kind,par,lhs,rhs=r
    return {'kind':kind,'k':par if kind=='fix' else None,'rng':par if kind=='range' else None,
            'lhs':strip_outer_parens(lhs),'rhs':strip_outer_parens(rhs)}

def parse_seq_chain(s:str):
    t=sanitize_spaces(strip_outer_parens(s)); terms=[]; delays=[]; cur=t
    while True:
        r=_find_top_level_seqop(cur)
        if not r: break
        kind,par,lhs,rhs=r
        terms.append(strip_outer_parens(rhs))
        delays.append(('fix',par) if kind=='fix' else ('range',par))
        cur=strip_outer_parens(lhs)
    terms.append(cur); terms.reverse(); delays.reverse()
    return None if not delays else (terms,delays)

def chain_to_string(terms,delays):
    def ensure_parens(t):
        t = t.strip()
        if t.startswith('(') and t.endswith(')'):
            return t
        return f"({t})"
    out = [ensure_parens(terms[0])]
    for i, d in enumerate(delays, 1):
        kind, par = d
        if kind == 'fix':
            out.append(f"##{par}")
        else:
            m, n = par
            out.append(f"##{m}" if m == n else f"##[{m}:{n}]")
        out.append(ensure_parens(terms[i]))
    return " ".join(out)

def _range_adj_or_overlap(r1,r2):
    (m1,n1)=(r1); (m2,n2)=(r2); return not (n1+1<m2 or n2+1<m1)
def _range_union(r1,r2): return (min(r1[0],r2[0]),max(r1[1],r2[1]))
def _make_seq(kind,par,lhs,rhs):
    if kind=='fix': return f"{lhs} ##{par} {rhs}"
    m,n=par; return f"{lhs} ##{m} {rhs}" if m==n else f"{lhs} ##[{m}:{n}] {rhs}"

def factor_temporal_or(children:List[Node],verbose=False)->Optional[str]:
    ks=[to_string(ch) for ch in children]
    items=[(i,_seqrepr(s)) for i,s in enumerate(ks)]
    changed=False; used=set(); new=[]
    groups={}
    for idx,d in items:
        if d is None: continue
        key=('lhs',d['kind'], d['k'] if d['kind']=='fix' else d['rng'], d['lhs'])
        groups.setdefault(key,[]).append((idx,d))
    for key,its in groups.items():
        if len(its)>=2:
            rhs_terms=[it[1]['rhs'] for it in its]
            rhs_s=to_string(deep_simplify(Node("OR",[parse_boolean_expr(x) for x in rhs_terms])))
            kind=its[0][1]['kind']; par=its[0][1]['k'] if kind=='fix' else its[0][1]['rng']; lhs=its[0][1]['lhs']
            new.append(_make_seq(kind,par,lhs,rhs_s)); used.update(i for i,_ in its); changed=True
    groups={}
    for idx,d in items:
        if d is None: continue
        if idx in used: continue
        key=('rhs',d['kind'], d['k'] if d['kind']=='fix' else d['rng'], d['rhs'])
        groups.setdefault(key,[]).append((idx,d))
    for key,its in groups.items():
        if len(its)>=2:
            lhs_terms=[it[1]['lhs'] for it in its]
            lhs_s=to_string(deep_simplify(Node("OR",[parse_boolean_expr(x) for x in lhs_terms])))
            kind=its[0][1]['kind']; par=its[0][1]['k'] if kind=='fix' else its[0][1]['rng']; rhs=its[0][1]['rhs']
            new.append(_make_seq(kind,par,lhs_s,rhs)); used.update(i for i,_ in its); changed=True
    lr={}
    for idx,d in items:
        if d is None: continue
        if idx in used: continue
        key=(d['lhs'],d['rhs']); lr.setdefault(key,[]).append((idx,d))
    for key,its in lr.items():
        if len(its)>=2:
            inter=[]
            for idx,d in its:
                inter.append((idx,(d['k'],d['k'])) if d['kind']=='fix' else (idx,d['rng']))
            inter.sort(key=lambda x:x[1][0])
            cm,cn=inter[0][1]; idxs=[inter[0][0]]; ok=True
            for j in range(1,len(inter)):
                _,(m,n)=inter[j]
                if _range_adj_or_overlap((cm,cn),(m,n)):
                    cm,cn=_range_union((cm,cn),(m,n)); idxs.append(inter[j][0])
                else: ok=False; break
            if ok:
                lhs,rhs=key; new.append(_make_seq('range',(cm,cn),lhs,rhs))
                used.update(idxs); changed=True
    chains=[]
    for idx,s in enumerate(ks):
        if idx in used: continue
        ch=parse_seq_chain(s)
        if ch: chains.append((idx,ch))
    sig={}
    for idx,(terms,delays) in chains:
        if len(terms)>=2:
            key=(tuple(terms[:-1]),tuple(delays))
            sig.setdefault(key,[]).append((idx,terms[-1]))
    for key,its in sig.items():
        if len(its)>=2:
            last=to_string(deep_simplify(Node("OR",[parse_boolean_expr(t) for _,t in its])))
            prefix=list(key[0])+[last]; delays=list(key[1])
            new.append(chain_to_string(prefix,delays)); used.update(i for i,_ in its); changed=True
    if not changed: return None
    rem=[ks[i] for i,_ in enumerate(children) if i not in used]; all_terms=rem+new
    if not all_terms: return "false"
    if len(all_terms)==1: return all_terms[0]
    return "(" + " || ".join(all_terms) + ")"

def factor_temporal_and(children:List[Node],verbose=False)->Optional[str]:
    ks=[to_string(ch) for ch in children]
    items=[(i,_seqrepr(s)) for i,s in enumerate(ks)]
    changed=False; used=set(); new=[]
    groups={}
    for idx,d in items:
        if not d or d['kind']!='fix': continue
        key=('lhs',d['k'],d['lhs']); groups.setdefault(key,[]).append((idx,d))
    for key,its in groups.items():
        if len(its)>=2:
            rh=[it[1]['rhs'] for it in its]
            rhs_s=to_string(deep_simplify(Node("AND",[parse_boolean_expr(x) for x in rh])))
            k=its[0][1]['k']; lhs=its[0][1]['lhs']; new.append(_make_seq('fix',k,lhs,rhs_s))
            used.update(i for i,_ in its); changed=True
    groups={}
    for idx,d in items:
        if not d or d['kind']!='fix': continue
        if idx in used: continue
        key=('rhs',d['k'],d['rhs']); groups.setdefault(key,[]).append((idx,d))
    for key,its in groups.items():
        if len(its)>=2:
            lh=[it[1]['lhs'] for it in its]
            lhs_s=to_string(deep_simplify(Node("AND",[parse_boolean_expr(x) for x in lh])))
            k=its[0][1]['k']; rhs=its[0][1]['rhs']; new.append(_make_seq('fix',k,lhs_s,rhs))
            used.update(i for i,_ in its); changed=True
    chains=[]
    for idx,s in enumerate(ks):
        if idx in used: continue
        ch=parse_seq_chain(s)
        if ch: chains.append((idx,ch))
    sig={}
    for idx,(terms,delays) in chains:
        if len(terms)>=2:
            key=(tuple(terms[:-1]),tuple(delays))
            sig.setdefault(key,[]).append((idx,terms[-1]))
    for key,its in sig.items():
        if len(its)>=2:
            last=to_string(deep_simplify(Node("AND",[parse_boolean_expr(t) for _,t in its])))
            prefix=list(key[0])+[last]; delays=list(key[1])
            new.append(chain_to_string(prefix,delays)); used.update(i for i,_ in its); changed=True
    if not changed: return None
    rem=[ks[i] for i,_ in enumerate(children) if i not in used]; all_terms=rem+new
    if not all_terms: return "true"
    if len(all_terms)==1: return all_terms[0]
    return "(" + " && ".join(all_terms) + ")"

def to_string(n:Node)->str:
    if n.op=="TRUE": return "true"
    if n.op=="FALSE":return "false"
    if n.op=="ATOM":
        s=n.atom
        return s if (s.startswith('(') and s.endswith(')')) else f"({s})"
    if n.op=="NOT":
        c=n.kids[0]; return "!" + (to_string(c) if c.op in ("ATOM","TRUE","FALSE") else "(" + to_string(c) + ")")
    if n.op=="AND": return "(" + " && ".join(to_string(c) for c in n.kids) + ")"
    if n.op=="OR":  return "(" + " || ".join(to_string(c) for c in n.kids) + ")"
    raise RuntimeError("unknown node")

def simplify_once(n:Node)->Node:
    if n.op in ("TRUE","FALSE","ATOM"): return n
    if n.op=="NOT":
        x=n.kids[0]
        if   x.op=="TRUE":  return Node("FALSE")
        elif x.op=="FALSE": return Node("TRUE")
        return n
    n=flatten_assoc(n); n=remove_dups_and_sort(n)
    if n.op=="AND":
        if any(c.op=="FALSE" for c in n.kids): return Node("FALSE")
        kids=[c for c in n.kids if c.op!="TRUE"]
        if not kids: return Node("TRUE")
        if contains_complement_pair({canonical_key(c):c for c in kids}): return Node("FALSE")
        top={canonical_key(c):c for c in kids if c.op!="OR"}; new=[]
        for c in kids:
            if c.op=="OR":
                or_map={canonical_key(x):x for x in c.kids}
                if any(k in or_map for k in top): continue
            new.append(c)
        n=Node("AND",new); n=remove_dups_and_sort(flatten_assoc(n))
        fact=factor_temporal_and(n.kids,verbose=RULE_LOGS)
        if fact is not None:
            n2=parse_boolean_expr(fact); n2=remove_dups_and_sort(flatten_assoc(n2))
            if len(n2.kids)==1 and n2.op in ("AND","OR"): return n2.kids[0]
            return n2
        if len(n.kids)==1: return n.kids[0]
        return n
    if n.op=="OR":
        if any(c.op=="TRUE" for c in n.kids): return Node("TRUE")
        kids=[c for c in n.kids if c.op!="FALSE"]
        if not kids: return Node("FALSE")
        if contains_complement_pair({canonical_key(c):c for c in kids}): return Node("TRUE")
        top={canonical_key(c):c for c in kids if c.op!="AND"}; new=[]
        for c in kids:
            if c.op=="AND":
                and_map={canonical_key(x):x for x in c.kids}
                if any(k in and_map for k in top): continue
            new.append(c)
        n=Node("OR",new); n=remove_dups_and_sort(flatten_assoc(n))
        fact=factor_temporal_or(n.kids,verbose=RULE_LOGS)
        if fact is not None:
            n2=parse_boolean_expr(fact); n2=remove_dups_and_sort(flatten_assoc(n2))
            if len(n2.kids)==1 and n2.op in ("AND","OR"): return n2.kids[0]
            return n2
        if len(n.kids)==1: return n.kids[0]
        return n
    return n

def deep_simplify(n:Node, trace:bool=False, label:str="")->Node:
    prev=None; cur=to_nnf(n)
    def rec(x:Node)->Node:
        if x.op in ("ATOM","TRUE","FALSE"): return x
        if x.op=="NOT": return Node("NOT",[rec(x.kids[0])])
        return Node(x.op,[rec(c) for c in x.kids])
    while True:
        cur=rec(cur); cur=simplify_once(cur); key=canonical_key(cur)
        if prev==key: return cur
        prev=key

def canonicalize_for_key(expr:str)->str:
    return canonical_key(deep_simplify(parse_boolean_expr(expr),trace=False))

def _find_top_level_hashhash_fix(t:str):
    d=0; i=0
    while i<len(t)-1:
        c=t[i]
        if c=='(' : d+=1
        elif c==')': d-=1
        elif c=='#' and i+1<len(t) and t[i+1]=='#' and d==0:
            m=_RANGE_RE.match(t[i:])
            if m and m.start()==0: return None
            j=i+2; m2=re.match(r"\s*(\d+)\s*",t[j:])
            if not m2: return None
            k=int(m2.group(1)); jn=j+m2.end(); return (i,k,jn)
        i+=1
    return None

def expr_to_timeline(expr:str)->Optional[Dict[int,Node]]:
    t=sanitize_spaces(expr)
    if _RANGE_RE.search(t): return None
    hh=_find_top_level_hashhash_fix(t)
    if not hh: return {0: parse_boolean_expr(t)}
    i,k,jn=hh; lhs=t[:i].strip(); rhs=t[jn:].strip()
    tl=expr_to_timeline(lhs); tr=expr_to_timeline(rhs)
    if tl is None or tr is None: return None
    out:Dict[int,Node]={}
    for off,n in tl.items(): out[off]=Node("AND",[out[off],n]) if off in out else n
    for off,n in tr.items():
        noff=off+k; out[noff]=Node("AND",[out[noff],n]) if noff in out else n
    return out

def _collect_k_vectors_if_same_skeleton_and_all_fix(pres:List[str]):
    chains=[]
    for p in pres:
        ch=parse_seq_chain(p)
        if not ch: return None
        terms, delays = ch
        for kind, par in delays:
            if kind!='fix': return None
        chains.append((terms, tuple(d[1] for d in delays)))
    base_terms=chains[0][0]
    for t,_ in chains:
        if t!=base_terms: return None
    k_vectors=[kv for _,kv in chains]
    return base_terms, k_vectors

def _is_cartesian_product_contiguous(k_vectors:List[Tuple[int,...]])->Optional[List[Tuple[int,int]]]:
    if not k_vectors: return None
    m=len(k_vectors[0])
    for kv in k_vectors:
        if len(kv)!=m: return None
    per_dim=[]
    for d in range(m):
        vals=sorted({kv[d] for kv in k_vectors})
        ok=all(vals[i]+1==vals[i+1] for i in range(len(vals)-1))
        if not ok: return None
        per_dim.append((vals[0], vals[-1]))
    prod=1
    for lo,hi in per_dim: prod *= (hi-lo+1)
    if prod != len(set(k_vectors)): return None
    return per_dim

def _single_varying_dim_range(base_terms:List[str], k_vectors:List[Tuple[int,...]])->Optional[Tuple[int,Tuple[int,int]]]:
    if not k_vectors: return None
    m=len(k_vectors[0])
    same_dims=[True]*m
    for d in range(m):
        vals={kv[d] for kv in k_vectors}
        if len(vals)==1: continue
        same_dims[d]=False
    vary=[i for i,b in enumerate(same_dims) if not b]
    if len(vary)!=1: return None
    d=vary[0]
    vals=sorted({kv[d] for kv in k_vectors})
    ok=all(vals[i]+1==vals[i+1] for i in range(len(vals)-1))
    if not ok: return None
    return d,(vals[0],vals[-1])

def _apply_interval_merge_to_term(term: str) -> str:
    try:
        term_node = parse_boolean_expr(term)
        merged_node = _merge_intervals_in_or(term_node)
        simplified_node = deep_simplify(merged_node, trace=False)
        return to_string(simplified_node)
    except:
        return term

def _merge_intervals_in_temporal(node: Node) -> Node:
    if node.op != "ATOM":
        if node.op in ("AND", "OR"):
            return Node(node.op, [_merge_intervals_in_temporal(child) for child in node.kids])
        elif node.op == "NOT":
            return Node("NOT", [_merge_intervals_in_temporal(node.kids[0])])
        else:
            return node
    text = node.atom
    if "##" not in text:
        try:
            sub_node = parse_boolean_expr(text)
            merged = _merge_intervals_in_or(sub_node)
            simplified = deep_simplify(merged, trace=False)
            return Node("ATOM", atom=to_string(simplified))
        except:
            return node
    chain = parse_seq_chain(text)
    if chain is None:
        return node
    terms, delays = chain
    new_terms = []
    for term in terms:
        merged_term = _apply_interval_merge_to_term(term)
        new_terms.append(merged_term)
    new_text = chain_to_string(new_terms, delays)
    return Node("ATOM", atom=new_text)

def _merge_intervals_in_or(or_node: Node) -> Node:
    if or_node.op != "OR":
        return or_node
    intervals_by_var: Dict[str, List[Tuple[Optional[int], Optional[int], List[Node]]]] = {}
    other_terms = []
    for child in or_node.kids:
        if child.op == "AND":
            bounds: Dict[str, Dict[str, Optional[int]]] = {}
            others_in_and = []
            for sub in child.kids:
                if sub.op == "ATOM":
                    info = _parse_ineq_atom(sub.atom)
                    if info:
                        sig, kind, val = info
                        bounds.setdefault(sig, {'ge': None, 'le': None})
                        if kind == 'ge':
                            if bounds[sig]['ge'] is None or val > bounds[sig]['ge']:
                                bounds[sig]['ge'] = val
                        else:
                            if bounds[sig]['le'] is None or val < bounds[sig]['le']:
                                bounds[sig]['le'] = val
                    else:
                        others_in_and.append(sub)
                else:
                    others_in_and.append(sub)
            if len(bounds) == 1 and not others_in_and:
                var = list(bounds.keys())[0]
                ge_val = bounds[var]['ge']
                le_val = bounds[var]['le']
                intervals_by_var.setdefault(var, []).append((ge_val, le_val, others_in_and))
            else:
                other_terms.append(child)
        elif child.op == "ATOM":
            info = _parse_ineq_atom(child.atom)
            if info:
                sig, kind, val = info
                if kind == 'ge':
                    intervals_by_var.setdefault(sig, []).append((val, None, []))
                else:
                    intervals_by_var.setdefault(sig, []).append((None, val, []))
            else:
                other_terms.append(child)
        else:
            other_terms.append(child)
    merged_terms = []
    for var, intervals in intervals_by_var.items():
        if len(intervals) <= 1:
            for ge_val, le_val, others in intervals:
                and_terms = []
                if ge_val is not None:
                    and_terms.append(Node("ATOM", atom=f"{var} >= {ge_val}"))
                if le_val is not None:
                    and_terms.append(Node("ATOM", atom=f"{var} <= {le_val}"))
                and_terms.extend(others)
                if len(and_terms) == 1:
                    merged_terms.append(and_terms[0])
                elif len(and_terms) > 1:
                    merged_terms.append(Node("AND", and_terms))
        else:
            first_others = intervals[0][2]
            all_others_same = all(
                len(others) == len(first_others) and
                all(canonical_key(o1) == canonical_key(o2) for o1, o2 in zip(others, first_others))
                for _, _, others in intervals
            )
            if all_others_same:
                all_ge = [ge for ge, _, _ in intervals if ge is not None]
                all_le = [le for _, le, _ in intervals if le is not None]
                merged_ge = min(all_ge) if all_ge else None
                merged_le = max(all_le) if all_le else None
                if merged_ge is not None and merged_le is not None and merged_ge > merged_le:
                    for ge_val, le_val, others in intervals:
                        and_terms = []
                        if ge_val is not None:
                            and_terms.append(Node("ATOM", atom=f"{var} >= {ge_val}"))
                        if le_val is not None:
                            and_terms.append(Node("ATOM", atom=f"{var} <= {le_val}"))
                        and_terms.extend(others)
                        if len(and_terms) == 1:
                            merged_terms.append(and_terms[0])
                        elif len(and_terms) > 1:
                            merged_terms.append(Node("AND", and_terms))
                else:
                    and_terms = []
                    if merged_ge is not None:
                        and_terms.append(Node("ATOM", atom=f"{var} >= {merged_ge}"))
                    if merged_le is not None:
                        and_terms.append(Node("ATOM", atom=f"{var} <= {merged_le}"))
                    and_terms.extend(first_others)
                    if len(and_terms) == 1:
                        merged_terms.append(and_terms[0])
                    elif len(and_terms) > 1:
                        merged_terms.append(Node("AND", and_terms))
            else:
                for ge_val, le_val, others in intervals:
                    and_terms = []
                    if ge_val is not None:
                        and_terms.append(Node("ATOM", atom=f"{var} >= {ge_val}"))
                    if le_val is not None:
                        and_terms.append(Node("ATOM", atom=f"{var} <= {le_val}"))
                    and_terms.extend(others)
                    if len(and_terms) == 1:
                        merged_terms.append(and_terms[0])
                    elif len(and_terms) > 1:
                        merged_terms.append(Node("AND", and_terms))
    all_terms = merged_terms + other_terms
    if not all_terms:
        return Node("FALSE")
    if len(all_terms) == 1:
        return all_terms[0]
    return Node("OR", all_terms)

def _get_temporal_structure_signature(pre: str) -> Tuple:
    chain = parse_seq_chain(pre)
    if chain is None:
        return (False, 0, ())
    terms, delays = chain
    delay_info = tuple((kind, par) for kind, par in delays)
    return (True, len(delays), delay_info)

def _strip_redundant_outer_parens(text: str) -> str:
    text = text.strip()
    if not (text.startswith("(") and text.endswith(")")):
        return text
    depth = 0
    for i, ch in enumerate(text):
        if ch == '(': depth += 1
        elif ch == ')':
            depth -= 1
            if depth == 0 and i != len(text) - 1:
                return text
    inner = text[1:-1].strip()
    if "##" in inner or "&&" in inner or "||" in inner or inner.startswith("!"):
        return inner
    return text

def merge_same_post(lines: List[str], verbose: bool = False, trace_simpl: bool = False) -> List[str]:
    groups: Dict[Tuple[str, str], List[str]] = {}
    post_repr: Dict[Tuple[str, str], str] = {}
    for ln in lines:
        parsed = parse_one_sva(ln)
        if not parsed: continue
        clock, pre, post = parsed
        post_key = canonicalize_for_key(post)
        k = (clock, post_key)
        groups.setdefault(k, []).append(pre)
        if k not in post_repr:
            post_repr[k] = to_string(deep_simplify(parse_boolean_expr(post), trace=False))
    merged = []
    for gi, ((clock, _), pres) in enumerate(groups.items()):
        post_text = post_repr[(clock, _)]
        temporal_buckets: Dict[Tuple, List[str]] = {}
        for pre in pres:
            sig = _get_temporal_structure_signature(pre)
            temporal_buckets.setdefault(sig, []).append(pre)
        for bucket_idx, (bucket_sig, bucket_pres) in enumerate(temporal_buckets.items()):
            safe = _collect_k_vectors_if_same_skeleton_and_all_fix(bucket_pres)
            pre_node = None
            if safe is not None:
                base_terms, k_vectors = safe
                per_dim = _is_cartesian_product_contiguous(k_vectors)
                if per_dim is not None:
                    delays=[('range',(lo,hi)) if lo!=hi else ('fix',lo) for (lo,hi) in per_dim]
                    pre_node = parse_boolean_expr(chain_to_string(base_terms,delays))
                else:
                    sv = _single_varying_dim_range(base_terms,k_vectors)
                    if sv is not None:
                        d,(lo,hi)=sv
                        fixed = list(k_vectors[0])
                        delays=[('fix',fixed[i]) for i in range(len(fixed))]
                        delays[d]=('range',(lo,hi))
                        pre_node = parse_boolean_expr(chain_to_string(base_terms,delays))
            if pre_node is None:
                pre_node = Node("OR", [parse_boolean_expr(p) for p in bucket_pres]) if len(bucket_pres)>1 else parse_boolean_expr(bucket_pres[0])
            simplified = deep_simplify(pre_node, trace=trace_simpl, label=f"post#{gi}.{bucket_idx}")
            temporal_merged = _merge_intervals_in_temporal(simplified)
            interval_merged = _merge_intervals_in_or(temporal_merged)
            interval_text_before = to_string(simplified)
            interval_text_after = to_string(interval_merged)
            if interval_text_after != interval_text_before:
                simplified = deep_simplify(interval_merged, trace=False, label=f"post#{gi}.{bucket_idx}.interval")
            pre_text = to_string(simplified)
            pre_text = _strip_redundant_outer_parens(pre_text)
            post_text_stripped = _strip_redundant_outer_parens(post_text)
            merged.append(f"assert property ({clock} {pre_text} |-> {post_text_stripped});".replace("( ","(").replace(" )",")"))
    return merged

def collect_atoms(n:Node,bag:Optional[List[str]]=None)->List[str]:
    if bag is None: bag=[]
    if n.op=="ATOM":
        if n.atom not in bag: bag.append(n.atom)
    elif n.op=="NOT": collect_atoms(n.kids[0],bag)
    elif n.op in ("AND","OR"):
        for c in n.kids: collect_atoms(c,bag)
    return bag

def symbolize(n:Node):
    atoms=collect_atoms(n); mp={a:f"P{i}" for i,a in enumerate(atoms)}
    def r(x:Node)->str:
        if x.op=="TRUE": return "true"
        if x.op=="FALSE":return "false"
        if x.op=="ATOM": return mp.get(x.atom,x.atom)
        if x.op=="NOT":
            ch=r(x.kids[0]); 
            return "!" + (ch if x.kids[0].op in ("ATOM","TRUE","FALSE") else f"({ch})")
        if x.op=="AND": return "(" + " && ".join(r(c) for c in x.kids) + ")"
        if x.op=="OR":  return "(" + " || ".join(r(c) for c in x.kids) + ")"
        raise RuntimeError("unknown")
    return r(n), mp

def load_lines(path:str)->List[str]:
    with open(path,"r",encoding="utf-8") as f: return [ln.strip() for ln in f if ln.strip()]

def build_indexed_assertions(lines:List[str])->Dict[int,str]: return {i:ln for i,ln in enumerate(lines)}

def main(sva_map: Dict[int, str]) -> Dict[int, str]:
    global RULE_LOGS
    RULE_LOGS = False
    lines: List[str] = [sva_map[k] for k in sorted(sva_map.keys())]
    merged: List[str] = merge_same_post(lines, verbose=False, trace_simpl=False)
    return {i: ln for i, ln in enumerate(merged)}

if __name__=="__main__":
    pass
