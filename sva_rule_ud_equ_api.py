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
                    elif rhs_depth == 0 and rhs_end + 1 < n:
                        if t[rhs_end:rhs_end+2] in ['||','&&']:
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
                    elif rhs_depth == 0 and rhs_end + 1 < n:
                        if t[rhs_end:rhs_end+2] in ['||','&&']:
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

def are_equivalent_by_spot(ltl1: str, ltl2: str) -> bool:
    if not HAVE_SPOT:
        return False
    try:
        f1 = spot.formula(ltl1)
        f2 = spot.formula(ltl2)
        if str(f1) == str(f2):
            return True
        xor_formula = spot.formula.Xor(f1, f2)
        xor_aut = xor_formula.translate()
        return xor_aut.is_empty()
    except Exception:
        return False

def deduplicate_with_mapping(sva_with_ids: list[tuple[int, str]], use_spot: bool = True):
    rows = []
    for sid, line in sva_with_ids:
        parsed = parse_one_sva(line)
        if not parsed:
            continue
        clock, pre, post = parsed
        ltl = translate_sva_to_ltl_G(pre, post)
        rows.append((sid, line, ltl))
    ltl_list = [ltl for _, _, ltl in rows]
    classes = []
    lightweight_hits = 0
    spot_hits = 0
    for i, fi in enumerate(ltl_list):
        matched = False
        for cls_pos, cls in enumerate(classes):
            rep_idx, _, members = cls
            rep_ltl = ltl_list[rep_idx]
            if fi == rep_ltl:
                classes[cls_pos][2].append(i)
                matched = True
                lightweight_hits += 1
                break
            if use_spot and HAVE_SPOT:
                if are_equivalent_by_spot(fi, rep_ltl):
                    classes[cls_pos][2].append(i)
                    matched = True
                    spot_hits += 1
                    break
        if not matched:
            classes.append((i, fi, [i]))
    id_to_rep = {}
    output_lines = []
    for cls_pos, cls in enumerate(classes):
        rep_idx, _, members = cls
        rep_sid = rows[rep_idx][0]
        rep_line = rows[rep_idx][1]
        output_lines.append(f"#{rep_sid}: {rep_line}")
        for mem_idx in members:
            if mem_idx != rep_idx:
                mem_sid = rows[mem_idx][0]
                id_to_rep[mem_sid] = rep_sid
    return output_lines, id_to_rep

def read_assertions_from_file(file_path: str) -> list[tuple[int, str]]:
    sva_with_ids = []
    with open(file_path, 'r') as file:
        lines = file.readlines()
        for idx, line in enumerate(lines, start=1):
            line = line.strip()
            if line.startswith("assert property"):
                sva_with_ids.append((idx, line))
    return sva_with_ids

def normalize_assertion_format(assertion: str) -> str:
    def remove_extra_parentheses(s: str) -> str:
        while True:
            new_s = re.sub(r'\(\s*\(([^()]+)\)\s*\)', r'(\1)', s)
            if new_s == s:
                break
            s = new_s
        return s
    assertion = remove_extra_parentheses(assertion)
    assertion = re.sub(r'\(\s*([A-Za-z_]\w*\s*(?:==|!=|<=|>=|<|>)\s*[A-Za-z0-9_\'bxh]+)\s*\)', r'\1', assertion)
    assertion = re.sub(r'\(\s*([A-Za-z_]\w*)\s*\)', r'\1', assertion)
    assertion = re.sub(r'([A-Za-z_]\w*\s*(?:==|!=|<=|>=|<|>)\s*[A-Za-z0-9_\'bxh]+)', r'(\1)', assertion)
    return assertion

def main(sva_map: dict[int, str]) -> dict[int, str]:
    result, _ = main_with_visit_tracking(sva_map)
    return result

def main_with_visit_tracking(sva_map: dict[int, str]):
    sva_with_ids = [(k, v) for k, v in sorted(sva_map.items(), key=lambda x: x[0])]
    output_lines, rep_of = deduplicate_with_mapping(sva_with_ids)
    kept_indices = set()
    result = {}
    output_idx = 0
    for ln in output_lines:
        m = re.match(r"^#(\d+):\s*(.*)$", ln)
        if m:
            original_idx = int(m.group(1))
            content = m.group(2)
            kept_indices.add(original_idx)
        else:
            content = ln
        result[output_idx] = content
        output_idx += 1
    input_indices = set(sva_map.keys())
    deleted_indices = input_indices - kept_indices
    return result, deleted_indices

if __name__ == "__main__":
    pass
