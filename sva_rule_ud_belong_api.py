# -*- coding: utf-8 -*-
import re
try:
    import spot
    HAVE_SPOT = True
except Exception:
    spot = None
    HAVE_SPOT = False

def sanitize_spaces(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip())

def strip_outer_parens(t: str) -> str:
    t = t.strip()
    while t.startswith('(') and t.endswith(')'):
        depth = 0
        ok = True
        for i, ch in enumerate(t):
            if ch == '(':
                depth += 1
            elif ch == ')':
                depth -= 1
                if depth == 0 and i != len(t) - 1:
                    ok = False
                    break
        if ok:
            t = t[1:-1].strip()
        else:
            break
    return t

def split_top_level(t: str, op_char: str) -> list[str]:
    parts, depth, last = [], 0, 0
    i = 0
    while i < len(t):
        c = t[i]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        elif depth == 0 and c == op_char:
            parts.append(t[last:i].strip())
            last = i + 1
        i += 1
    parts.append(t[last:].strip())
    return [p for p in parts if p != ""]

def next_pow(s: str, k: int) -> str:
    s = sanitize_spaces(s)
    for _ in range(k):
        s = f"X({s})"
    return s

def find_one_top_level_hashhash(t: str):
    t = t.strip()
    depth = 0
    i = 0
    n = len(t)
    while i < n - 1:
        c = t[i]
        if c == '(':
            depth += 1
        elif c == ')':
            depth -= 1
        elif c == '#' and i + 1 < n and t[i + 1] == '#' and depth == 0:
            j = i + 2
            m1 = re.match(r"\s*\[\s*(\d+)\s*:\s*(\d+)\s*\]\s*", t[j:])
            if m1:
                mval, nval = int(m1.group(1)), int(m1.group(2))
                jj = j + m1.end()
                lhs = t[:i].strip()
                rhs_start = jj
                rhs_depth = 0
                rhs_end = rhs_start
                while rhs_end < n:
                    ch = t[rhs_end]
                    if ch == '(':
                        rhs_depth += 1
                    elif ch == ')':
                        rhs_depth -= 1
                    elif rhs_depth == 0:
                        if rhs_end + 1 < n:
                            two_ch = t[rhs_end:rhs_end+2]
                            if two_ch in ['||', '&&']:
                                break
                    rhs_end += 1
                rhs = t[rhs_start:rhs_end].strip()
                return (i, rhs_end, lhs, ("rng", mval, nval), rhs)
            m2 = re.match(r"\s*(\d+)\s*", t[j:])
            if m2:
                kval = int(m2.group(1))
                jj = j + m2.end()
                lhs = t[:i].strip()
                rhs_start = jj
                rhs_depth = 0
                rhs_end = rhs_start
                while rhs_end < n:
                    ch = t[rhs_end]
                    if ch == '(':
                        rhs_depth += 1
                    elif ch == ')':
                        rhs_depth -= 1
                    elif rhs_depth == 0:
                        if rhs_end + 1 < n:
                            two_ch = t[rhs_end:rhs_end+2]
                            if two_ch in ['||', '&&']:
                                break
                    rhs_end += 1
                rhs = t[rhs_start:rhs_end].strip()
                return (i, rhs_end, lhs, ("fix", kval), rhs)
            return None
        i += 1
    return None

def strip_leading_hashhash(s: str):
    s = s.lstrip()
    s = strip_outer_parens(s)
    if not s.startswith("##"):
        return (None, None, None, s)
    j = 2
    m1 = re.match(r"\s*\[\s*(\d+)\s*:\s*(\d+)\s*\]\s*(.*)$", s[j:], flags=re.DOTALL)
    if m1:
        mval, nval = int(m1.group(1)), int(m1.group(2))
        rest = m1.group(3).strip()
        return ("rng", mval, nval, rest)
    m2 = re.match(r"\s*(\d+)\s*(.*)$", s[j:], flags=re.DOTALL)
    if m2:
        kval = int(m2.group(1))
        rest = m2.group(2).strip()
        return ("fix", kval, None, rest)
    return (None, None, None, s)

def expand_all_seq_to_x(expr: str) -> str:
    s = sanitize_spaces(expr)
    max_iterations = 50
    for iteration in range(max_iterations):
        original = s
        s = strip_outer_parens(s)
        while s.endswith('))') and s.count('(') < s.count(')'):
            s = s[:-1]
        s_strip = strip_outer_parens(s)
        kind, m, n, rest = strip_leading_hashhash(s_strip)
        if kind == "fix":
            s = next_pow(sanitize_spaces(rest), m)
            continue
        elif kind == "rng":
            inner = sanitize_spaces(rest)
            terms = [next_pow(inner, k) for k in range(m, n + 1)]
            s = "(" + " | ".join(terms) + ")"
            continue
        hh = find_one_top_level_hashhash(s)
        if hh:
            i, jj, lhs, kind, rhs = hh
            lhs_e = sanitize_spaces(lhs)
            rhs_e = sanitize_spaces(rhs)
            if kind[0] == "fix":
                rep = f"(({lhs_e}) & {next_pow(rhs_e, kind[1])})"
            else:
                mval, nval = kind[1], kind[2]
                rhs_or = "(" + " | ".join(next_pow(rhs_e, k) for k in range(mval, nval + 1)) + ")"
                rep = f"(({lhs_e}) & {rhs_or})"
            s = rep + " " + s[jj:]
            continue
        if '|' in s or '&' in s:
            parts_or = split_top_level(s, '|')
            if len(parts_or) > 1:
                expanded_parts = []
                any_changed = False
                for part in parts_or:
                    expanded = expand_all_seq_to_x(part)
                    expanded_parts.append(expanded)
                    if expanded != part:
                        any_changed = True
                if any_changed:
                    s = "(" + " | ".join(expanded_parts) + ")"
                    continue
            parts_and = split_top_level(s, '&')
            if len(parts_and) > 1:
                expanded_parts = []
                any_changed = False
                for part in parts_and:
                    expanded = expand_all_seq_to_x(part)
                    expanded_parts.append(expanded)
                    if expanded != part:
                        any_changed = True
                if any_changed:
                    s = "(" + " & ".join(expanded_parts) + ")"
                    continue
        if s == original:
            break
    def fix_parens(u: str) -> str:
        u = u.strip()
        balance = 0
        out = []
        for ch in u:
            if ch == '(':
                balance += 1
                out.append(ch)
            elif ch == ')':
                if balance > 0:
                    balance -= 1
                    out.append(ch)
            else:
                out.append(ch)
        out_s = ''.join(out)
        out_s += ')' * balance
        return out_s
    s = fix_parens(s)
    return s

def translate_boolean(expr: str) -> str:
    expr = sanitize_spaces(expr)
    def tr(t: str) -> str:
        t = sanitize_spaces(t).replace("&&", "&").replace("||", "|")
        t = strip_outer_parens(t)
        if t.startswith('!'):
            return f"!({tr(t[1:].strip())})"
        ors = split_top_level(t, '|')
        if len(ors) > 1:
            parts = sorted(tr(p) for p in ors)
            return "(" + " | ".join(parts) + ")"
        ands = split_top_level(t, '&')
        if len(ands) > 1:
            parts = sorted(tr(p) for p in ands)
            return "(" + " & ".join(parts) + ")"
        return t if t else "true"
    return tr(expr)

_AP_GLOBAL: dict[str, str] = {}
RE_ATOMIC_CMP = re.compile(
    r"""\(
         \s*
         (?!ap\d+\s*\))
         [^()&|!^X]*?
         (?:==|!=|<=|>=|<|>)
         [^()&|!^X]*?
     \)
     """, re.VERBOSE)

RE_BARE_CMP = re.compile(
    r"(?<![A-Za-z0-9_])"
    r"(?!ap\d+\b)"
    r"([A-Za-z_]\w*\s*(?:==|!=|<=|>=|<|>)\s*[A-Za-z0-9_'bxh]+)"
)

def normalize_atomic_propositions_many(ltl_list: list[str]) -> list[str]:
    global _AP_GLOBAL
    _AP_GLOBAL = {}
    def map_one_formula(s: str) -> str:
        out = sanitize_spaces(s)
        while True:
            changed = False
            def repl(m: re.Match) -> str:
                nonlocal changed
                full = m.group(0)
                key = sanitize_spaces(full)
                if key.startswith("(ap") and key.endswith(")"):
                    return full
                name = _AP_GLOBAL.get(key)
                if name is None:
                    name = f"ap{len(_AP_GLOBAL)}"
                    _AP_GLOBAL[key] = name
                changed = True
                return name
            new_out = sanitize_spaces(RE_ATOMIC_CMP.sub(repl, out))
            if not changed:
                def repl_bare(mb: re.Match) -> str:
                    nonlocal changed
                    token = sanitize_spaces(mb.group(1))
                    key = f"({token})"
                    name = _AP_GLOBAL.get(key)
                    if name is None:
                        name = f"ap{len(_AP_GLOBAL)}"
                        _AP_GLOBAL[key] = name
                    changed = True
                    return name
                new_out2 = sanitize_spaces(RE_BARE_CMP.sub(repl_bare, new_out))
                if not changed:
                    break
                out = new_out2
            else:
                out = new_out
        out = re.sub(r"\(\s*\(([^()]+)\)\s*\)", r"(\1)", out)
        return strip_outer_parens(out)
    return [map_one_formula(s) for s in ltl_list]

ASSERT_RE = re.compile(
    r"assert\s+property\s*\(\s*(?:(@\s*\([^)]+\))\s*)?(?P<pre>.+?)\s*\|\->\s*(?P<post>.+?)\s*\)\s*;",
    re.IGNORECASE | re.DOTALL)

def parse_one_sva(line: str):
    m = ASSERT_RE.search(line)
    if not m:
        return None
    clock = m.group(1) or ""
    pre = m.group("pre").strip()
    post = m.group("post").strip()
    return clock, pre, post

def translate_sva_to_ltl_G(pre: str, post: str) -> str:
    def preprocess_expr(expr: str) -> str:
        expanded = expand_all_seq_to_x(expr)
        boolean = translate_boolean(expanded)
        return boolean
    pre_ltl = preprocess_expr(pre)
    post_ltl = preprocess_expr(post)
    result = f"G( ({pre_ltl}) -> ({post_ltl}) )"
    return result

def _has_temporal(s: str) -> bool:
    return "##" in s

class _Var:
    def __init__(self, name): self.name = name
class _Not:
    def __init__(self, x): self.x = x
class _And:
    def __init__(self, a, b): self.a, self.b = a, b
class _Or:
    def __init__(self, a, b): self.a, self.b = a, b

def _tok_bool(s):
    TOK=[(r'\s+',None),(r'\&\&','AND'),(r'\|\|','OR'),(r'\!','NOT'),(r'\(','LP'),(r'\)','RP'),(r"[^!\s\&\|\(\)]+",'ATOM')]
    pos=0
    while pos<len(s):
        for pat,typ in TOK:
            m=re.match(pat,s[pos:])
            if m:
                if typ: yield (typ,m.group(0))
                pos+=len(m.group(0)); break
        else:
            raise SyntaxError("bad token")

class _P:
    def __init__(self,toks): self.t=list(toks); self.i=0
    def pk(self): return self.t[self.i] if self.i<len(self.t) else None
    def eat(self,typ):
        x=self.pk()
        if not x or x[0]!=typ: raise SyntaxError("expect "+typ)
        self.i+=1; return x
    def parse(self): return self.p_or()
    def p_or(self):
        n=self.p_and()
        while self.pk() and self.pk()[0]=='OR': self.eat('OR'); n=_Or(n,self.p_and())
        return n
    def p_and(self):
        n=self.p_un()
        while self.pk() and self.pk()[0]=='AND': self.eat('AND'); n=_And(n,self.p_un())
        return n
    def p_un(self):
        if self.pk() and self.pk()[0]=='NOT': self.eat('NOT'); return _Not(self.p_un())
        if self.pk() and self.pk()[0]=='LP': self.eat('LP'); n=self.p_or(); self.eat('RP'); return n
        return _Var(self.eat('ATOM')[1])

def _normalize_bool_ops(s:str)->str:
    s = s.replace('&&', '&&').replace('||', '||')
    s = s.replace('&&', '\u2026AND\u2026').replace('||', '\u2026OR\u2026')
    s = s.replace('&', '&&').replace('|', '||')
    s = s.replace('\u2026AND\u2026', '&&').replace('\u2026OR\u2026', '||')
    return s

def _parse_bool(s): return _P(_tok_bool(_normalize_bool_ops(s))).parse()

def _to_nnf(n):
    if isinstance(n, _Var): return n
    if isinstance(n, _Not):
        x=n.x
        if isinstance(x,_Var): return n
        if isinstance(x,_Not): return _to_nnf(x.x)
        if isinstance(x,_And): return _Or(_to_nnf(_Not(x.a)), _to_nnf(_Not(x.b)))
        if isinstance(x,_Or):  return _And(_to_nnf(_Not(x.a)), _to_nnf(_Not(x.b)))
    if isinstance(n,_And): return _And(_to_nnf(n.a), _to_nnf(n.b))
    if isinstance(n,_Or):  return _Or(_to_nnf(n.a), _to_nnf(n.b))

def _dist(a,b):
    if isinstance(a,_And): return _And(_dist(a.a,b), _dist(a.b,b))
    if isinstance(b,_And): return _And(_dist(a,b.a), _dist(a,b.b))
    return _Or(a,b)

def _to_cnf(n):
    n=_to_nnf(n)
    if isinstance(n,(_Var,_Not)): return n
    if isinstance(n,_And): return _And(_to_cnf(n.a), _to_cnf(n.b))
    if isinstance(n,_Or):  return _dist(_to_cnf(n.a), _to_cnf(n.b))

def _flat_and(n): return _flat_and(n.a)+_flat_and(n.b) if isinstance(n,_And) else [n]
def _flat_or(n):  return _flat_or(n.a)+_flat_or(n.b) if isinstance(n,_Or) else [n]

def _lit_str(x):
    if isinstance(x,_Var): return x.name
    if isinstance(x,_Not): return "!"+_lit_str(x.x)
    raise TypeError

def _cnf_clauses(n):
    cnf=_to_cnf(n); cls=[]
    for c in _flat_and(cnf):
        lits=set(_lit_str(x) for x in _flat_or(c))
        if any((v in lits and f"!{v}" in lits) for v in lits): continue
        cls.append(frozenset(sorted(lits)))
    return frozenset(cls)

def _implies_by_cnf(a:str,b:str)->bool:
    try:
        n=_And(_parse_bool(a), _Not(_parse_bool(b)))
        return not _cnf_clauses(n)
    except Exception:
        return False

_RE_ID = re.compile(r"\b[A-Za-z_]\w*\b")
def _to_py_expr(s:str)->str:
    s=_normalize_bool_ops(s)
    s=s.replace('&&',' and ').replace('||',' or ')
    s=re.sub(r"!\s*\(", " not (", s)
    s=re.sub(r"!\s*([A-Za-z_]\w*)", r" not \1", s)
    return s

def _vars_in(s:str)->list[str]:
    vs=sorted(set(_RE_ID.findall(s)))
    return [v for v in vs if v not in ("true","false","and","or","not")]

def _eval_bool_expr(expr:str, env:dict)->bool:
    py=_to_py_expr(expr)
    try:
        return bool(eval(py, {"__builtins__":{}}, env))
    except Exception:
        return False

def _bool_implies(a:str,b:str)->bool:
    vs=sorted(set(_vars_in(a)+_vars_in(b)))
    if len(vs)>10:
        return False
    n=len(vs)
    for mask in range(1<<n):
        env={vs[i]: bool((mask>>i)&1) for i in range(n)}
        if _eval_bool_expr(a, env) and not _eval_bool_expr(b, env):
            return False
    return True

def _ltl_implies(f: str, g: str, use_spot: bool = True) -> tuple[bool, str]:
    if f == g:
        return (True, "轻量化")
    if use_spot and HAVE_SPOT:
        try:
            ff = spot.formula(f)
            gg = spot.formula(g)
            h = spot.formula.And(ff, spot.formula.Not(gg))
            aut = spot.translate(h)
            result = aut.is_empty()
            return (result, "Spot")
        except Exception:
            pass
    return (False, "轻量化")

def deduplicate_with_mapping(sva_with_ids: list[tuple[int, str]], use_spot: bool = True):
    rows=[]
    for sid, line in sva_with_ids:
        parsed = parse_one_sva(line)
        if not parsed:
            continue
        _, pre, post = parsed
        ltl = translate_sva_to_ltl_G(pre, post)
        pre_bool = translate_boolean(pre)
        post_bool = translate_boolean(post)
        has_tmp = _has_temporal(pre) or _has_temporal(post)
        pre_src = sanitize_spaces(strip_outer_parens(pre))
        post_src = sanitize_spaces(strip_outer_parens(post))
        rows.append((sid, line, ltl, pre_bool, post_bool, has_tmp, pre_src, post_src))
    n = len(rows)
    removed = [False] * n
    rep_of = {}
    stats = {"轻量化命中":0,"Spot命中":0,"总比较次数":0}
    for i in range(n):
        if removed[i]:
            continue
        _, _, fi, pre_i, post_i, tmp_i, pre_src_i, post_src_i = rows[i]
        for j in range(n):
            if i == j or removed[j]:
                continue
            _, _, fj, pre_j, post_j, tmp_j, pre_src_j, post_src_j = rows[j]
            stats["总比较次数"] += 1
            decided=False
            if not tmp_i and not tmp_j and pre_i == pre_j:
                if _bool_implies(post_i, post_j) or _implies_by_cnf(post_i, post_j):
                    removed[j] = True
                    rep_of[rows[j][0]] = rows[i][0]
                    stats["轻量化命中"] += 1
                    decided=True
                elif (_bool_implies(post_j, post_i) or _implies_by_cnf(post_j, post_i)) and not removed[i]:
                    removed[i] = True
                    rep_of[rows[i][0]] = rows[j][0]
                    stats["轻量化命中"] += 1
                    decided=True
                    break
            if decided:
                continue
            if pre_src_i == pre_src_j:
                if _bool_implies(post_src_i, post_src_j):
                    removed[j] = True
                    rep_of[rows[j][0]] = rows[i][0]
                    stats["轻量化命中"] += 1
                    continue
                if _bool_implies(post_src_j, post_src_i) and not removed[i]:
                    removed[i] = True
                    rep_of[rows[i][0]] = rows[j][0]
                    stats["轻量化命中"] += 1
                    break
            result_ij, method_ij = _ltl_implies(fi, fj, use_spot)
            if result_ij:
                removed[j] = True
                rep_of[rows[j][0]] = rows[i][0]
                if method_ij == "Spot":
                    stats["Spot命中"] += 1
                else:
                    stats["轻量化命中"] += 1
                continue
            result_ji, method_ji = _ltl_implies(fj, fi, use_spot)
            if result_ji and not removed[i]:
                removed[i] = True
                rep_of[rows[i][0]] = rows[j][0]
                if method_ji == "Spot":
                    stats["Spot命中"] += 1
                else:
                    stats["轻量化命中"] += 1
                break
    output_lines=[]
    for idx, (sid, line, _, _, _, _, _, _) in enumerate(rows):
        if not removed[idx]:
            output_lines.append(f"#{sid}: {line}")
    reps=[]
    for ln in output_lines:
        m = re.match(r"^#\d+:\s*(.*)$", ln)
        reps.append(m.group(1) if m else ln)
    return reps, rep_of, stats

def main(sva_map: dict[int, str], use_spot: bool = True) -> dict[int, str]:
    sva_list = [sva_map[k] for k in sorted(sva_map.keys())]
    sva_with_ids = [(i, s) for i, s in enumerate(sva_list)]
    reps, _, stats = deduplicate_with_mapping(sva_with_ids, use_spot)
    return {i: s for i, s in enumerate(reps)}

if __name__ == "__main__":
    pass
