import re
from typing import List, Tuple, Dict, Optional

ASSERT_RE = re.compile(
    r"assert\s+property\s*\(\s*(?:(@\s*\([^)]+\))\s*)?(?P<pre>.+?)\s*\|\->\s*(?P<post>.+?)\s*\)\s*;",
    re.IGNORECASE | re.DOTALL,
)

def sanitize_spaces(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip())

def parse_one_sva(line: str) -> Optional[Tuple[str, str, str]]:
    m = ASSERT_RE.search(line)
    if not m:
        return None
    return (m.group(1) or ""), m.group("pre").strip(), m.group("post").strip()

class Node:
    __slots__ = ("op", "kids", "atom")
    def __init__(self, op: str, kids: Optional[List['Node']] = None, atom: Optional[str] = None):
        self.op = op
        self.kids = kids or []
        self.atom = atom
    def __repr__(self):
        if self.op == "ATOM":
            return f"ATOM({self.atom})"
        if self.op in ("TRUE", "FALSE"):
            return self.op
        if self.op == "NOT":
            return f"NOT({self.kids[0]!r})"
        return f"{self.op}({', '.join(repr(k) for k in self.kids)})"

def make_atom(s: str) -> Node:
    s = sanitize_spaces(s)
    if s.lower() == "true" or s == "":
        return Node("TRUE")
    if s.lower() == "false":
        return Node("FALSE")
    return Node("ATOM", atom=s)

def strip_outer_parens(t: str) -> str:
    t = t.strip()
    while t.startswith('(') and t.endswith(')'):
        d = 0
        ok = True
        for i, ch in enumerate(t):
            if ch == '(':
                d += 1
            elif ch == ')':
                d -= 1
                if d == 0 and i != len(t) - 1:
                    ok = False
                    break
        if ok:
            t = t[1:-1].strip()
        else:
            break
    return t

def split_top_level(t: str, ch: str) -> List[str]:
    parts = []
    d = 0
    last = 0
    i = 0
    while i < len(t):
        c = t[i]
        if c == '(':
            d += 1
        elif c == ')':
            d -= 1
        elif d == 0 and c == ch:
            parts.append(t[last:i].strip())
            last = i + 1
        i += 1
    parts.append(t[last:].strip())
    return [p for p in parts if p != ""]

def parse_boolean_expr(text: str) -> Node:
    t = sanitize_spaces(text).replace("&&", "&").replace("||", "|")
    if "##" in t:
        return make_atom(t)
    t = strip_outer_parens(t)
    while t.startswith('!'):
        return Node("NOT", [parse_boolean_expr(t[1:].strip())])
    ors = split_top_level(t, '|')
    if len(ors) > 1:
        return Node("OR", [parse_boolean_expr(p) for p in ors])
    ands = split_top_level(t, '&')
    if len(ands) > 1:
        return Node("AND", [parse_boolean_expr(p) for p in ands])
    return make_atom(t)

def to_nnf(n: Node) -> Node:
    if n.op == "NOT":
        x = n.kids[0]
        if x.op == "NOT":
            return to_nnf(x.kids[0])
        if x.op == "AND":
            return Node("OR", [to_nnf(Node("NOT", [c])) for c in x.kids])
        if x.op == "OR":
            return Node("AND", [to_nnf(Node("NOT", [c])) for c in x.kids])
        if x.op == "TRUE":
            return Node("FALSE")
        if x.op == "FALSE":
            return Node("TRUE")
        return Node("NOT", [to_nnf(x)])
    if n.op in ("AND", "OR"):
        return Node(n.op, [to_nnf(c) for c in n.kids])
    return n

def canonical_key(n: Node) -> str:
    if n.op == "ATOM":
        return f"A:{n.atom}"
    if n.op == "TRUE":
        return "T"
    if n.op == "FALSE":
        return "F"
    if n.op == "NOT":
        return f"N:{canonical_key(n.kids[0])}"
    ks = sorted(canonical_key(c) for c in n.kids)
    return f"{n.op}(" + ",".join(ks) + ")"

def flatten_assoc(n: Node) -> Node:
    if n.op not in ("AND", "OR"):
        return n
    out = []
    for c in n.kids:
        if c.op == n.op:
            out.extend(c.kids)
        else:
            out.append(c)
    return Node(n.op, out)

def remove_dups_and_sort(n: Node) -> Node:
    if n.op not in ("AND", "OR"):
        return n
    uniq = {canonical_key(c): c for c in n.kids}
    return Node(n.op, [uniq[k] for k in sorted(uniq)])

def contains_complement_pair(kset: Dict[str, Node]) -> bool:
    atoms = set()
    negs = set()
    for _, c in kset.items():
        if c.op == "ATOM":
            atoms.add(canonical_key(c))
        elif c.op == "NOT" and c.kids[0].op == "ATOM":
            negs.add(canonical_key(c.kids[0]))
    return any(a in negs for a in atoms)

_RANGE_RE = re.compile(r"\#\#\s*\[\s*(\d+)\s*:\s*(\d+)\s*\]")

def _find_top_level_seqop(t: str):
    t = sanitize_spaces(t)
    d = 0
    i = 0
    while i < len(t) - 1:
        c = t[i]
        if c == '(':
            d += 1
        elif c == ')':
            d -= 1
        elif c == '#' and i + 1 < len(t) and t[i + 1] == '#' and d == 0:
            m = _RANGE_RE.match(t[i:])
            if m and m.start() == 0:
                mval = int(m.group(1))
                nval = int(m.group(2))
                lhs = t[:i].strip()
                rhs = t[i + m.end():].strip()
                return ('range', (mval, nval), lhs, rhs)
            j = i + 2
            m2 = re.match(r"\s*(\d+)\s*", t[j:])
            if m2:
                kval = int(m2.group(1))
                lhs = t[:i].strip()
                rhs = t[j + m2.end():].strip()
                return ('fix', kval, lhs, rhs)
            return None
        i += 1
    return None

def _seqrepr(s: str):
    s = strip_outer_parens(s)
    r = _find_top_level_seqop(s)
    if not r:
        return None
    kind, par, lhs, rhs = r
    return {
        'kind': kind,
        'k': par if kind == 'fix' else None,
        'rng': par if kind == 'range' else None,
        'lhs': strip_outer_parens(lhs),
        'rhs': strip_outer_parens(rhs)
    }

def parse_seq_chain(s: str):
    t = sanitize_spaces(strip_outer_parens(s))
    terms = []
    delays = []
    cur = t
    while True:
        r = _find_top_level_seqop(cur)
        if not r:
            break
        kind, par, lhs, rhs = r
        terms.append(strip_outer_parens(rhs))
        delays.append(('fix', par) if kind == 'fix' else ('range', par))
        cur = strip_outer_parens(lhs)
    terms.append(cur)
    terms.reverse()
    delays.reverse()
    return None if not delays else (terms, delays)

def chain_to_string(terms, delays):
    out = [terms[0]]
    for i, d in enumerate(delays, 1):
        kind, par = d
        if kind == 'fix':
            out.append(f"##{par}")
        else:
            m, n = par
            out.append(f"##{m}" if m == n else f"##[{m}:{n}]")
        out.append(terms[i])
    return " ".join(out)

def _range_adj_or_overlap(r1, r2):
    (m1, n1) = r1
    (m2, n2) = r2
    return not (n1 + 1 < m2 or n2 + 1 < m1)

def _range_union(r1, r2):
    return (min(r1[0], r2[0]), max(r1[1], r2[1]))

def _make_seq(kind, par, lhs, rhs):
    if kind == 'fix':
        return f"{lhs} ##{par} {rhs}"
    m, n = par
    return f"{lhs} ##{m} {rhs}" if m == n else f"{lhs} ##[{m}:{n}] {rhs}"

def factor_temporal_or(children: List[Node]) -> Optional[str]:
    ks = [to_string(ch) for ch in children]
    items = [(i, _seqrepr(s)) for i, s in enumerate(ks)]
    changed = False
    used = set()
    new = []
    groups = {}
    for idx, d in items:
        if d is None:
            continue
        key = ('lhs', d['kind'], d['k'] if d['kind'] == 'fix' else d['rng'], d['lhs'])
        groups.setdefault(key, []).append((idx, d))
    for key, its in groups.items():
        if len(its) >= 2:
            rhs_terms = [it[1]['rhs'] for it in its]
            rhs_s = to_string(deep_simplify(Node("OR", [parse_boolean_expr(x) for x in rhs_terms])))
            kind = its[0][1]['kind']
            par = its[0][1]['k'] if kind == 'fix' else its[0][1]['rng']
            lhs = its[0][1]['lhs']
            m = _make_seq(kind, par, lhs, rhs_s)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    groups = {}
    for idx, d in items:
        if d is None:
            continue
        key = ('rhs', d['kind'], d['k'] if d['kind'] == 'fix' else d['rng'], d['rhs'])
        groups.setdefault(key, []).append((idx, d))
    for key, its in groups.items():
        if len(its) >= 2:
            lhs_terms = [it[1]['lhs'] for it in its]
            lhs_s = to_string(deep_simplify(Node("OR", [parse_boolean_expr(x) for x in lhs_terms])))
            kind = its[0][1]['kind']
            par = its[0][1]['k'] if kind == 'fix' else its[0][1]['rng']
            rhs = its[0][1]['rhs']
            m = _make_seq(kind, par, lhs_s, rhs)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    lr = {}
    for idx, d in items:
        if d is None:
            continue
        key = (d['lhs'], d['rhs'])
        lr.setdefault(key, []).append((idx, d))
    for key, its in lr.items():
        if len(its) >= 2:
            inter = []
            for idx, d in its:
                inter.append((idx, (d['k'], d['k'])) if d['kind'] == 'fix' else (idx, d['rng']))
            inter.sort(key=lambda x: x[1][0])
            cm, cn = inter[0][1]
            idxs = [inter[0][0]]
            ok = True
            for j in range(1, len(inter)):
                _, (m, n) = inter[j]
                if _range_adj_or_overlap((cm, cn), (m, n)):
                    cm, cn = _range_union((cm, cn), (m, n))
                    idxs.append(inter[j][0])
                else:
                    ok = False
                    break
            if ok:
                lhs, rhs = key
                m = _make_seq('range', (cm, cn), lhs, rhs)
                new.append(m)
                used.update(idxs)
                changed = True
    chains = []
    for idx, s in enumerate(ks):
        ch = parse_seq_chain(s)
        if ch:
            chains.append((idx, ch))
    sig = {}
    for idx, (terms, delays) in chains:
        if len(terms) >= 2:
            key = (tuple(terms[:-1]), tuple(delays))
            sig.setdefault(key, []).append((idx, terms[-1]))
    for key, its in sig.items():
        if len(its) >= 2:
            last = to_string(deep_simplify(Node("OR", [parse_boolean_expr(t) for _, t in its])))
            prefix = list(key[0]) + [last]
            delays = list(key[1])
            m = chain_to_string(prefix, delays)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    if not changed:
        return None
    rem = [ks[i] for i, _ in enumerate(children) if i not in used]
    all_terms = rem + new
    if not all_terms:
        return "false"
    if len(all_terms) == 1:
        return all_terms[0]
    return "(" + " || ".join(all_terms) + ")"

def factor_temporal_and(children: List[Node]) -> Optional[str]:
    ks = [to_string(ch) for ch in children]
    items = [(i, _seqrepr(s)) for i, s in enumerate(ks)]
    changed = False
    used = set()
    new = []
    groups = {}
    for idx, d in items:
        if not d or d['kind'] != 'fix':
            continue
        key = ('lhs', d['k'], d['lhs'])
        groups.setdefault(key, []).append((idx, d))
    for key, its in groups.items():
        if len(its) >= 2:
            rh = [it[1]['rhs'] for it in its]
            rhs_s = to_string(deep_simplify(Node("AND", [parse_boolean_expr(x) for x in rh])))
            k = its[0][1]['k']
            lhs = its[0][1]['lhs']
            m = _make_seq('fix', k, lhs, rhs_s)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    groups = {}
    for idx, d in items:
        if not d or d['kind'] != 'fix':
            continue
        key = ('rhs', d['k'], d['rhs'])
        groups.setdefault(key, []).append((idx, d))
    for key, its in groups.items():
        if len(its) >= 2:
            lh = [it[1]['lhs'] for it in its]
            lhs_s = to_string(deep_simplify(Node("AND", [parse_boolean_expr(x) for x in lh])))
            k = its[0][1]['k']
            rhs = its[0][1]['rhs']
            m = _make_seq('fix', k, lhs_s, rhs)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    chains = []
    for idx, s in enumerate(ks):
        ch = parse_seq_chain(s)
        if ch:
            chains.append((idx, ch))
    sig = {}
    for idx, (terms, delays) in chains:
        if len(terms) >= 2:
            key = (tuple(terms[:-1]), tuple(delays))
            sig.setdefault(key, []).append((idx, terms[-1]))
    for key, its in sig.items():
        if len(its) >= 2:
            last = to_string(deep_simplify(Node("AND", [parse_boolean_expr(t) for _, t in its])))
            prefix = list(key[0]) + [last]
            delays = list(key[1])
            m = chain_to_string(prefix, delays)
            new.append(m)
            used.update(i for i, _ in its)
            changed = True
    if not changed:
        return None
    rem = [ks[i] for i, _ in enumerate(children) if i not in used]
    all_terms = rem + new
    if not all_terms:
        return "true"
    if len(all_terms) == 1:
        return all_terms[0]
    return "(" + " && ".join(all_terms) + ")"

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

def _tighten_numeric_bounds_in_and(and_node: Node) -> Node:
    assert and_node.op == "AND"
    others = []
    bounds: Dict[str, Dict[str, Optional[int]]] = {}
    for c in and_node.kids:
        if c.op == "ATOM":
            info = _parse_ineq_atom(c.atom)
            if not info:
                others.append(c)
                continue
            sig, kind, val = info
            bounds.setdefault(sig, {'ge': None, 'le': None})
            if kind == 'ge':
                if bounds[sig]['ge'] is None or val > bounds[sig]['ge']:
                    bounds[sig]['ge'] = val
            else:
                if bounds[sig]['le'] is None or val < bounds[sig]['le']:
                    bounds[sig]['le'] = val
        else:
            others.append(c)
    tk = []
    tk.extend(others)
    for sig, lr in bounds.items():
        lb, ub = lr['ge'], lr['le']
        if lb is not None and ub is not None and lb > ub:
            return Node("FALSE")
        if lb is not None:
            tk.append(make_atom(f"{sig} >= {lb}"))
        if ub is not None:
            tk.append(make_atom(f"{sig} <= {ub}"))
    if not tk:
        return Node("TRUE")
    if len(tk) == 1:
        return tk[0]
    return Node("AND", tk)

def to_string(n: Node) -> str:
    if n.op == "TRUE":
        return "true"
    if n.op == "FALSE":
        return "false"
    if n.op == "ATOM":
        s = n.atom
        return s if (s.startswith('(') and s.endswith(')')) else f"({s})"
    if n.op == "NOT":
        c = n.kids[0]
        return "!" + (to_string(c) if c.op in ("ATOM", "TRUE", "FALSE") else "(" + to_string(c) + ")")
    if n.op == "AND":
        kids_str = [to_string(c) for c in n.kids if c.op != "TRUE"]
        if not kids_str:
            return "true"
        if len(kids_str) == 1:
            return kids_str[0]
        return "(" + " && ".join(kids_str) + ")"
    if n.op == "OR":
        kids_str = [to_string(c) for c in n.kids if c.op != "FALSE"]
        if not kids_str:
            return "false"
        if len(kids_str) == 1:
            return kids_str[0]
        return "(" + " || ".join(kids_str) + ")"
    raise RuntimeError("unknown node")

def simplify_once(n: Node) -> Node:
    if n.op in ("TRUE", "FALSE", "ATOM"):
        return n
    if n.op == "NOT":
        x = n.kids[0]
        if x.op == "TRUE":
            return Node("FALSE")
        elif x.op == "FALSE":
            return Node("TRUE")
        return n
    n = flatten_assoc(n)
    n = remove_dups_and_sort(n)
    if n.op == "AND":
        if any(c.op == "FALSE" for c in n.kids):
            return Node("FALSE")
        kids = [c for c in n.kids if c.op != "TRUE"]
        if not kids:
            return Node("TRUE")
        if contains_complement_pair({canonical_key(c): c for c in kids}):
            return Node("FALSE")
        top = {canonical_key(c): c for c in kids if c.op != "OR"}
        new = []
        for c in kids:
            if c.op == "OR":
                or_map = {canonical_key(x): x for x in c.kids}
                if any(k in or_map for k in top):
                    continue
            new.append(c)
        n = Node("AND", new)
        n = remove_dups_and_sort(flatten_assoc(n))
        n = _tighten_numeric_bounds_in_and(n)
        if n.op in ("TRUE", "FALSE", "ATOM"):
            return n
        fact = factor_temporal_and(n.kids)
        if fact is not None:
            n2 = parse_boolean_expr(fact)
            n2 = remove_dups_and_sort(flatten_assoc(n2))
            if len(n2.kids) == 1 and n2.op in ("AND", "OR"):
                return n2.kids[0]
            return n2
        if len(n.kids) == 1:
            return n.kids[0]
        return n
    if n.op == "OR":
        if any(c.op == "TRUE" for c in n.kids):
            return Node("TRUE")
        kids = [c for c in n.kids if c.op != "FALSE"]
        if not kids:
            return Node("FALSE")
        if contains_complement_pair({canonical_key(c): c for c in kids}):
            return Node("TRUE")
        top = {canonical_key(c): c for c in kids if c.op != "AND"}
        new = []
        for c in kids:
            if c.op == "AND":
                and_map = {canonical_key(x): x for x in c.kids}
                if any(k in and_map for k in top):
                    continue
            new.append(c)
        n = Node("OR", new)
        n = remove_dups_and_sort(flatten_assoc(n))
        fact = factor_temporal_or(n.kids)
        if fact is not None:
            n2 = parse_boolean_expr(fact)
            n2 = remove_dups_and_sort(flatten_assoc(n2))
            if len(n2.kids) == 1 and n2.op in ("AND", "OR"):
                return n2.kids[0]
            return n2
        if len(n.kids) == 1:
            return n.kids[0]
        return n
    return n

def deep_simplify(n: Node) -> Node:
    prev = None
    cur = to_nnf(n)
    def rec(x: Node) -> Node:
        if x.op in ("ATOM", "TRUE", "FALSE"):
            return x
        if x.op == "NOT":
            return Node("NOT", [rec(x.kids[0])])
        return Node(x.op, [rec(c) for c in x.kids])
    while True:
        cur = rec(cur)
        cur = simplify_once(cur)
        key = canonical_key(cur)
        if prev == key:
            return cur
        prev = key

def canonicalize_for_key(expr: str) -> str:
    return canonical_key(deep_simplify(parse_boolean_expr(expr)))

def _find_top_level_hashhash_fix(t: str):
    d = 0
    i = 0
    while i < len(t) - 1:
        c = t[i]
        if c == '(':
            d += 1
        elif c == ')':
            d -= 1
        elif c == '#' and i + 1 < len(t) and t[i + 1] == '#' and d == 0:
            m = _RANGE_RE.match(t[i:])
            if m and m.start() == 0:
                return None
            j = i + 2
            m2 = re.match(r"\s*(\d+)\s*", t[j:])
            if not m2:
                return None
            k = int(m2.group(1))
            jn = j + m2.end()
            return (i, k, jn)
        i += 1
    return None

def expr_to_timeline(expr: str) -> Optional[Dict[int, Node]]:
    t = sanitize_spaces(expr)
    if _RANGE_RE.search(t):
        return None
    hh = _find_top_level_hashhash_fix(t)
    if not hh:
        return {0: parse_boolean_expr(t)}
    i, k, jn = hh
    lhs = t[:i].strip()
    rhs = t[jn:].strip()
    tl = expr_to_timeline(lhs)
    tr = expr_to_timeline(rhs)
    if tl is None or tr is None:
        return None
    out: Dict[int, Node] = {}
    for off, n in tl.items():
        out[off] = Node("AND", [out[off], n]) if off in out else n
    for off, n in tr.items():
        noff = off + k
        out[noff] = Node("AND", [out[noff], n]) if noff in out else n
    return out

def timeline_and_merge(exprs: List[str]) -> Optional[Dict[int, Node]]:
    merged: Dict[int, Node] = {}
    for e in exprs:
        tl = expr_to_timeline(e)
        if tl is None:
            return None
        for off, n in tl.items():
            merged[off] = Node("AND", [merged[off], n]) if off in merged else n
    for off in list(merged):
        merged[off] = deep_simplify(merged[off])
    return merged

def timeline_to_sva(tline: Dict[int, Node]) -> str:
    items = [(off, n) for off, n in tline.items() if to_string(n) not in ("true", "()")]
    if not items:
        return "true"
    items.sort(key=lambda x: x[0])
    parts = []
    cur_t, cur_phi = items[0]
    if cur_t > 0:
        parts.append(f"##{cur_t}")
    parts.append(to_string(cur_phi))
    for off, n in items[1:]:
        gap = off - cur_t
        parts.append(f"##{gap}")
        parts.append(to_string(n))
        cur_t = off
    return " ".join(parts)

_LEADING_FIX_RE = re.compile(r"^\s*(?:\(\s*true\s*\)\s*)?\#\#\s*(\d+)\b")
_LEADING_FIX_EXTRACT_RE = re.compile(r"^\s*\#\#\s*(\d+)\s+(.*)$", re.DOTALL)

def _leading_fix_offset_of_node(n: Node) -> int:
    s = to_string(n)
    s = strip_outer_parens(s)
    m = _LEADING_FIX_RE.match(s)
    return int(m.group(1)) if m else 0

def _extract_leading_fix(s: str):
    u = strip_outer_parens(s.strip())
    m = _LEADING_FIX_EXTRACT_RE.match(u)
    if not m:
        return None
    return int(m.group(1)), m.group(2).strip()

def _strip_redundant_outer_parens(text: str) -> str:
    text = text.strip()
    if not (text.startswith("(") and text.endswith(")")):
        return text
    depth = 0
    for i, ch in enumerate(text):
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
            if depth == 0 and i != len(text) - 1:
                return text
    inner = text[1:-1].strip()
    if "##" in inner or "&&" in inner or "||" in inner or inner.startswith("!"):
        return inner
    return text

def merge_same_pre(lines: List[str]) -> List[str]:
    groups: Dict[Tuple[str, str], List[str]] = {}
    pre_repr: Dict[Tuple[str, str], str] = {}
    for ln in lines:
        p = parse_one_sva(ln)
        if not p:
            continue
        clock, pre, post = p
        key = (clock, canonicalize_for_key(pre))
        groups.setdefault(key, []).append(post)
        if key not in pre_repr:
            pre_repr[key] = to_string(deep_simplify(parse_boolean_expr(pre)))
    out = []
    for (clock, _), posts in groups.items():
        pre_text = _strip_redundant_outer_parens(pre_repr[(clock, _)])
        tline = timeline_and_merge(posts)
        if tline is not None:
            post_node = parse_boolean_expr(timeline_to_sva(tline))
        else:
            post_node = Node("AND", [parse_boolean_expr(p) for p in posts]) if len(posts) > 1 else parse_boolean_expr(posts[0])
        simplified = deep_simplify(post_node)
        if simplified.op == "AND":
            simplified.kids.sort(key=_leading_fix_offset_of_node)
            t0_kids = []
            delayed = []
            for kid in simplified.kids:
                s = to_string(kid)
                off = _leading_fix_offset_of_node(kid)
                if off == 0:
                    t0_kids.append(kid)
                else:
                    ex = _extract_leading_fix(s)
                    if ex:
                        delayed.append(ex)
                    else:
                        delayed = None
                        break
            if delayed is not None and len(delayed) == 1:
                k, rest = delayed[0]
                t0_kids_filtered = [c for c in t0_kids if c.op != "TRUE" and to_string(c) not in ("true", "()")]
                if not t0_kids_filtered:
                    post_text = f"##{k} {rest}"
                else:
                    left = " && ".join(to_string(c) for c in t0_kids_filtered)
                    post_text = f"({left}) ##{k} {rest}"
                out.append(f"assert property ({clock} {pre_text} |-> {post_text});".replace("( ", "(").replace(" )", ")"))
                continue
        post_text = to_string(simplified)
        post_text = _strip_redundant_outer_parens(post_text)
        out.append(f"assert property ({clock} {pre_text} |-> {post_text});".replace("( ", "(").replace(" )", ")"))
    return out

def main(sva_map: Dict[int, str]) -> Dict[int, str]:
    lines: List[str] = [sva_map[k] for k in sorted(sva_map.keys())]
    merged: List[str] = merge_same_pre(lines)
    return {i: ln for i, ln in enumerate(merged)}
