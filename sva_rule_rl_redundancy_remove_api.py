#!/usr/bin/env python3

import re
from typing import List, Tuple, Set, Dict, Optional

def _balanced_paren(s: str) -> bool:
    d = 0
    for ch in s:
        if ch == '(':
            d += 1
        elif ch == ')':
            d -= 1
        if d < 0:
            return False
    return d == 0

def _strip_paren_once(s: str) -> str:
    s = s.strip()
    if s.startswith("(") and s.endswith(")"):
        inner = s[1:-1].strip()
        if _balanced_paren(inner):
            return inner
    return s

def _strip_paren_full(s: str) -> str:
    prev = None
    cur = s.strip()
    while prev != cur:
        prev = cur
        cur = _strip_paren_once(cur)
    return cur

def _norm_ap(s: str) -> str:
    return _strip_paren_full(s).replace(" ", "")

def _contains_temporal(s: str) -> bool:
    return "##" in s

def _extract_clock_and_condition(s: str) -> Tuple[Optional[str], str]:
    m = re.match(r'^(@\(posedge\s+\w+\))\s*(.*)$', s.strip())
    if m:
        return (m.group(1), m.group(2).strip())
    return (None, s.strip())

class Var:
    def __init__(self, name): 
        self.name = name
    def __repr__(self): 
        return self.name

class Not:
    def __init__(self, x): 
        self.x = x
    def __repr__(self): 
        return f"!{self.x}"

class And:
    def __init__(self, a, b): 
        self.a, self.b = a, b
    def __repr__(self): 
        return f"({self.a}&&{self.b})"

class Or:
    def __init__(self, a, b): 
        self.a, self.b = a, b
    def __repr__(self): 
        return f"({self.a}||{self.b})"

class Cmp:
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right
    def __repr__(self):
        op_map = {'GE': '>=', 'LE': '<=', 'EQ': '==', 'NE': '!=', 'GT': '>', 'LT': '<'}
        return f"({self.left} {op_map[self.op]} {self.right})"

def tokenize(s):
    TOKENS = [
        (r'\s+', None), 
        (r'\&\&', 'AND'), 
        (r'\|\|', 'OR'),
        (r'\!', 'NOT'), 
        (r'\(', 'LP'), 
        (r'\)', 'RP'),
        (r'>=', 'GE'), 
        (r'<=', 'LE'), 
        (r'==', 'EQ'), 
        (r'!=', 'NE'),
        (r'>', 'GT'), 
        (r'<', 'LT'),
        (r'[^!\s\&\|\(\)>=<!]+', 'ATOM'),
    ]
    pos = 0
    while pos < len(s):
        for pat, typ in TOKENS:
            m = re.match(pat, s[pos:])
            if m:
                if typ: 
                    yield (typ, m.group(0))
                pos += len(m.group(0))
                break
        else:
            raise SyntaxError(f"Bad token: {s[pos:pos+10]}")

class Parser:
    def __init__(self, toks): 
        self.toks, self.i = list(toks), 0
    def peek(self): 
        return self.toks[self.i] if self.i < len(self.toks) else None
    def eat(self, typ): 
        t = self.peek()
        if not t or t[0] != typ: 
            raise SyntaxError(f"Expected {typ}")
        self.i += 1
        return t
    def parse(self): 
        return self.parse_or()
    def parse_or(self):
        node = self.parse_and()
        while self.peek() and self.peek()[0] == 'OR':
            self.eat('OR')
            node = Or(node, self.parse_and())
        return node
    def parse_and(self):
        node = self.parse_unary()
        while self.peek() and self.peek()[0] == 'AND':
            self.eat('AND')
            node = And(node, self.parse_unary())
        return node
    def parse_unary(self):
        if self.peek() and self.peek()[0] == 'NOT':
            self.eat('NOT')
            return Not(self.parse_unary())
        if self.peek() and self.peek()[0] == 'LP':
            self.eat('LP')
            node = self.parse_or()
            self.eat('RP')
            return node
        if self.peek() and self.peek()[0] == 'ATOM':
            left = Var(self.eat('ATOM')[1])
            if self.peek() and self.peek()[0] in ['GE', 'LE', 'EQ', 'NE', 'GT', 'LT']:
                op = self.eat(self.peek()[0])[0]
                right = Var(self.eat('ATOM')[1])
                return Cmp(op, left, right)
            return left
        return Var(self.eat('ATOM')[1])

def parse_expr(s): 
    return Parser(tokenize(s)).parse()

def to_nnf(n):
    if isinstance(n, (Var, Cmp)): 
        return n
    if isinstance(n, Not):
        x = n.x
        if isinstance(x, (Var, Cmp)): 
            return n
        if isinstance(x, Not): 
            return to_nnf(x.x)
        if isinstance(x, And): 
            return Or(to_nnf(Not(x.a)), to_nnf(Not(x.b)))
        if isinstance(x, Or): 
            return And(to_nnf(Not(x.a)), to_nnf(Not(x.b)))
    if isinstance(n, And): 
        return And(to_nnf(n.a), to_nnf(n.b))
    if isinstance(n, Or): 
        return Or(to_nnf(n.a), to_nnf(n.b))
    raise TypeError

def distribute_or_over_and(a, b):
    if isinstance(a, And):
        return And(distribute_or_over_and(a.a, b), distribute_or_over_and(a.b, b))
    if isinstance(b, And):
        return And(distribute_or_over_and(a, b.a), distribute_or_over_and(a, b.b))
    return Or(a, b)

def to_cnf(n):
    n = to_nnf(n)
    if isinstance(n, (Var, Not, Cmp)): 
        return n
    if isinstance(n, And): 
        return And(to_cnf(n.a), to_cnf(n.b))
    if isinstance(n, Or): 
        return distribute_or_over_and(to_cnf(n.a), to_cnf(n.b))

def flatten_and(n): 
    return flatten_and(n.a) + flatten_and(n.b) if isinstance(n, And) else [n]

def flatten_or(n): 
    return flatten_or(n.a) + flatten_or(n.b) if isinstance(n, Or) else [n]

def lit_str(x):
    if isinstance(x, Var): 
        return x.name
    if isinstance(x, Not): 
        return "!" + lit_str(x.x)
    if isinstance(x, Cmp): 
        return f"{x.left.name}_{x.op}_{x.right.name}"
    raise TypeError

def cnf_clauses(n):
    cnf = to_cnf(n)
    clauses = []
    for c in flatten_and(cnf):
        lits = set(lit_str(x) for x in flatten_or(c))
        if any((v in lits and f"!{v}" in lits) for v in lits): 
            continue
        clauses.append(frozenset(sorted(lits)))
    return frozenset(clauses)

def eq_by_cnf(a, b):
    try: 
        return cnf_clauses(parse_expr(a)) == cnf_clauses(parse_expr(b))
    except: 
        return False

def implies_by_cnf(a, b):
    try:
        n = And(parse_expr(a), Not(parse_expr(b)))
        return not cnf_clauses(n)
    except: 
        return False

def simplify_boolean_line(pre, post):
    if eq_by_cnf(pre, post):
        return None
    if implies_by_cnf(pre, post):
        return None
    if eq_by_cnf(pre, "0") or eq_by_cnf(pre, "False"):
        return None
    if eq_by_cnf(post, "1") or eq_by_cnf(post, "true"):
        return None
    if eq_by_cnf(pre, "1") and eq_by_cnf(post, "0"):
        return "0"
    return post

def simplify_assert_line_temporal(line: str) -> str:
    return line

ASSERT_RE = re.compile(
    r"""assert\s+property\s*
        \(\s*(?P<pre>.*?)\s*\|\->\s*(?P<post>.*?)\s*\)
        \s*;""",
    re.IGNORECASE | re.DOTALL | re.VERBOSE
)

def simplify_assert_line(line: str) -> Optional[str]:
    line = line.strip()
    if not line:
        return None
    m = ASSERT_RE.search(line)
    if not m:
        return line
    pre_raw, post_raw = m.group("pre").strip(), m.group("post").strip()
    clock_pre, pre_cond = _extract_clock_and_condition(pre_raw)
    clock_post, post_cond = _extract_clock_and_condition(post_raw)
    if not _contains_temporal(pre_cond) and not _contains_temporal(post_cond):
        new_post = simplify_boolean_line(pre_cond, post_cond)
        if new_post is None:
            return None
        return f"assert property ({pre_raw} |-> {new_post});"
    return simplify_assert_line_temporal(line)

def simplify_text(assertions: list[str]) -> list[str]:
    out = []
    for assertion in assertions:
        assertion = assertion.strip()
        if not assertion: 
            continue
        res = simplify_assert_line(assertion)
        if res: 
            out.append(res)
    return out

def main(sva_map: Dict[int, str]) -> Dict[int, str]:
    assertions: List[str] = [sva_map[k] for k in sorted(sva_map.keys())]
    simplified: List[str] = simplify_text(assertions)
    return {i: s for i, s in enumerate(simplified)}

if __name__ == "__main__":
    pass
