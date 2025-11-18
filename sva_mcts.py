class VisitTracker:

    def __init__(self, initial_indices: Set[int]):
        self.visit_map: Dict[int, int] = {idx: 0 for idx in initial_indices}
        self.original_indices = set(initial_indices) 
    
    def mark_processed(self, indices: Set[int]):
        for idx in indices:
            if idx in self.visit_map:
                self.visit_map[idx] = 1
    
    def get_unvisited(self) -> Set[int]:
        return {idx for idx, v in self.visit_map.items() if v == 0}
    
    def get_visited(self) -> Set[int]:
        return {idx for idx, v in self.visit_map.items() if v == 1}
    
    def get_statistics(self) -> Dict[str, int]:
        total = len(self.original_indices)
        visited = sum(1 for v in self.visit_map.values() if v == 1)
        return {"total": total, "visited": visited, "unvisited": total - visited}
    
    def __repr__(self):
        stats = self.get_statistics()
        return f"<VisitTracker total={stats['total']} visited={stats['visited']} unvisited={stats['unvisited']}>"


normalize_sva_dict = lambda d: {i: l.strip() for i, l in sorted(d.items())}

def diff_dict(prev: Dict[int, str], curr: Dict[int, str]):
    prev_set, curr_set = set(prev.values()), set(curr.values())
    added   = [x for x in curr_set if x not in prev_set]
    removed = [x for x in prev_set if x not in curr_set]
    return added, removed


def api_dict2dict(api_func):
    return api_func

def api_list2dict(api_func):

    def _wrapped(sva_dict: Dict[int, str]) -> Dict[int, str]:

        sva_list = [sva_dict[k] for k in sorted(sva_dict.keys())]
        result_list = api_func(sva_list)

        return {i: s for i, s in enumerate(result_list)}
    return _wrapped


def sva_dict_tuple(d: dict[int, str]):

    values = []
    for k, v in d.items():
        assert isinstance(k, int), f"key must be int, got {type(k)}"
        assert isinstance(v, str), f"value must be str, got {type(v)} for key {k}"
        values.append(str(v))  
    return tuple(sorted(values))


ACTIONS: Dict[str, Callable[[Dict[int, str]], Dict[int, str]]] = {
    "belong":          belong_main,      
    "merge_same_pre":  merge_same_pre_main,
    "merge_same_post": merge_same_post_main,
    "redundancy":      remove_redundancy_main,
    "equ":             equ_main,
}

class SVAState:

    def __init__(self, sva_dict: Dict[int, str], history: Optional[list[str]] = None):
        normalized = normalize_sva_dict(sva_dict)
        fixed_dict = {}
        for i, l in normalized.items():
            assert isinstance(i, int), f"SVAState key must be int, got {type(i)}"
            assert isinstance(l, str), f"SVAState value must be str, got {type(l)}"
            fixed_dict[i] = str(l)
        
        self.svas: Dict[int, str] = fixed_dict
        self.svas_tuple = sva_dict_tuple(self.svas)
        self.history: list[str] = list(history) if history else []

    def spawn(self, new_dict: Dict[int, str], action_name: str) -> "SVAState":
        new_state = SVAState(new_dict, self.history + [action_name])
        return new_state

    def __hash__(self): 
        return hash(self.svas_tuple)

    def __eq__(self, other): 
        return isinstance(other, SVAState) and self.svas_tuple == other.svas_tuple

    def __repr__(self): 
        return f"<SVAState n={len(self.svas)} steps={len(self.history)}>"

REQUIRED_FIRST_ACTION: Optional[str] = None  

REQUIRE_NEXT: Dict[str, List[str]] = {}

ALLOW_ONLY_ONCE: set[str] = {
    "redundancy"
}

class Node:
    def __init__(self, state: SVAState, parent: Optional["Node"]=None, action: Optional[str]=None):
        self.state = state
        self.parent = parent
        self.action = action
        self.children: List["Node"] = []
        self.visits: int = 0
        self.value: float = 0.0
        self.tried_noop: set[str] = set()

    def is_fully_expanded(self, available_actions: List[str]) -> bool:
        tried = {ch.action for ch in self.children} | self.tried_noop
        return all(a in tried for a in available_actions)

    def best_child(self, c_param: float = 1.4) -> "Node":
        best_score, best_node = None, None
        for child in self.children:
            q = child.value / (child.visits + 1e-9)
            u = c_param * math.sqrt(math.log(self.visits + 1) / (child.visits + 1e-9))
            score = q + u
            if best_score is None or score > best_score:
                best_score, best_node = score, child
        return best_node

class MCTS:
    def __init__(self,
                 apply_rule_fn: Callable[[SVAState, str, "MCTS"], SVAState],
                 rule_selector_fn: Callable[[SVAState], List[str]],
                 evaluate_fn: Callable[[SVAState], float],
                 iteration_limit: int = 50,
                 c_param: float = 1.4,
                 seed: Optional[int] = None,
                 verbose: bool = True,
                 expensive_eval_fn: Optional[Callable[[SVAState], float]] = None,
                 k_expensive: Optional[int] = None,
                 reward_distribution: str = "uniform"
                 ):
        self.apply_rule_fn = apply_rule_fn
        self.rule_selector_fn = rule_selector_fn
        self.evaluate_fn = evaluate_fn
        self.iteration_limit = iteration_limit
        self.c_param = c_param
        self.verbose = verbose

        self.expensive_eval_fn = expensive_eval_fn
        self.k_expensive = k_expensive if (expensive_eval_fn and (k_expensive or 0) > 0) else None
        self.reward_distribution = reward_distribution

        self.best_state_seen: Optional[Tuple[float, SVAState]] = None
        self.memo: Dict[Tuple[Tuple[str, ...], str], Tuple[str, ...]] = {}
        self.exp_cache: Dict[Tuple[str, ...], float] = {}
        
        self.visit_tracker: Optional[VisitTracker] = None

        if seed is not None:
            random.seed(seed)

    def search(self, init_state: SVAState) -> SVAState:
        root = Node(init_state)
        
        self.visit_tracker = VisitTracker(set(init_state.svas.keys()))

        if self.k_expensive and init_state.svas_tuple not in self.exp_cache:
            pass

        for _ in range(1, self.iteration_limit + 1):
            node = self._select(root)
            expanded = self._expand(node)
            self._maybe_eval_and_backprop(expanded)

        if self.best_state_seen is not None:
            return self.best_state_seen[1]
        if not root.children:
            return root.state
        best = max(root.children, key=lambda n: n.visits)
        return best.state

    def _select(self, node: Node) -> Node:
        while True:
            actions = self.rule_selector_fn(node.state)
            if not node.children:
                return node
            if not node.is_fully_expanded(actions):
                return node
            node = node.best_child(self.c_param)

    def _expand(self, node: Node) -> Node:
        actions_all = self.rule_selector_fn(node.state)
        tried = {ch.action for ch in node.children} | node.tried_noop
        untried = [a for a in actions_all if a not in tried]
        random.shuffle(untried)

        for action in untried:
            key = (node.state.svas_tuple, action)
            if key in self.memo:
                new_svas_tuple = self.memo[key]
            else:
                new_state = self.apply_rule_fn(node.state, action, self)
                new_svas_tuple = tuple(str(s) for s in new_state.svas_tuple)
                self.memo[key] = new_svas_tuple

            if new_svas_tuple == node.state.svas_tuple:
                node.tried_noop.add(action)
                continue

            child_state = node.state.spawn({i: str(s) for i, s in enumerate(new_svas_tuple)}, action)
            child = Node(child_state, parent=node, action=action)
            node.children.append(child)
            return child
        return node

    def apply_rule_raw(self, svas: Tuple[str, ...], action_name: str) -> List[str]:
        fn = ACTIONS[action_name]
        return fn(list(svas))

    def _maybe_eval_and_backprop(self, node: Node):
        depth = len(node.state.history)
        proxy_score = self.evaluate_fn(node.state)

        if node.parent is not None:
            parent_score = self.evaluate_fn(node.parent.state)
            step_reward = proxy_score - parent_score
        else:
            step_reward = 0.0

        if step_reward != 0.0:
            self._backprop_range(node, steps=1, reward=step_reward, distribute="last")

        self._update_best(proxy_score, node.state)

        if self.k_expensive and depth > 0 and depth % self.k_expensive == 0:
            curr_exp = self._expensive_eval_cached(node.state)
            prev_anchor = self._ancestor_at_steps(node, self.k_expensive)
            prev_exp = self._expensive_eval_cached(prev_anchor.state)

            delta = curr_exp - prev_exp
            self._backprop_range(node, steps=self.k_expensive, reward=delta, distribute=self.reward_distribution)
            self._update_best(curr_exp, node.state)

    def _expensive_eval_cached(self, state: SVAState) -> float:
        key = state.svas_tuple
        if key in self.exp_cache:
            return self.exp_cache[key]
        if self.expensive_eval_fn is None:
            val = self.evaluate_fn(state)
        else:
            val = self.expensive_eval_fn(state)
        self.exp_cache[key] = val
        return val

    def _ancestor_at_steps(self, node: Node, steps: int) -> Node:
        cur, k = node, steps
        while k > 0 and cur.parent is not None:
            cur = cur.parent
            k -= 1
        return cur

    def _backprop_range(self, node: Node, steps: int, reward: float, distribute: str = "uniform"):
        if steps <= 0 or reward == 0.0:
            return
        if distribute == "last":
            n = node
            n.visits += 1
            n.value  += reward
            return

        portion = reward / float(steps)
        cur, k = node, steps
        while k > 0 and cur is not None:
            cur.visits += 1
            cur.value  += portion
            cur = cur.parent
            k -= 1

    def _update_best(self, score: float, state: SVAState):
        if (self.best_state_seen is None) or (score > self.best_state_seen[0]):
            self.best_state_seen = (score, state)

    def _print_delta(self, parent_state: SVAState, child_state: SVAState):
        added, removed = diff_dict(parent_state.svas, child_state.svas)
        
        old_count = len(parent_state.svas)
        new_count = len(child_state.svas)
        
        if old_count == new_count and len(added) == old_count and len(removed) == old_count:
            return
        
        if added:
            for _ in list(added)[:2]:
                pass
        if removed:
            for _ in list(removed)[:2]:
                pass

def apply_rule(state: SVAState, action_name: str, mcts: MCTS) -> SVAState:
    fn = ACTIONS[action_name]
    assert isinstance(state.svas, dict), f"state.svas must be dict, got {type(state.svas)}"
    for k, v in state.svas.items():
        assert isinstance(k, int), f"key must be int, got {type(k)}"
        assert isinstance(v, str), f"value must be str, got {type(v)}"

    old_indices = set(state.svas.keys())
    
    api_reported_indices = None
    if action_name == "equ":
        new_dict, api_reported_indices = equ_main_vt(state.svas)
    else:
        new_dict = fn(state.svas)

    assert isinstance(new_dict, dict), f"API {action_name} must return dict, got {type(new_dict)}"
    for k, v in new_dict.items():
        assert isinstance(k, int), f"API {action_name} key must be int, got {type(k)} for key {k}"
        assert isinstance(v, str), f"API {action_name} value must be str, got {type(v)} for key {k}"

    if normalize_sva_dict(new_dict) == state.svas:
        return state
    
    new_indices = set(new_dict.keys())
    disappeared_indices = old_indices - new_indices
    
    if api_reported_indices is not None:
        processed_indices = api_reported_indices
    else:
        processed_indices = disappeared_indices
    
    if mcts.visit_tracker and processed_indices:
        mcts.visit_tracker.mark_processed(processed_indices)
    
    return state.spawn(new_dict, action_name)

def rule_selector_with_constraints(state: SVAState) -> List[str]:
    base = list(ACTIONS.keys())
    h = state.history

    def _filter_once(xs: List[str]) -> List[str]:
        if not ALLOW_ONLY_ONCE:
            return xs
        used = set(h)
        return [a for a in xs if not (a in ALLOW_ONLY_ONCE and a in used)]

    if len(h) == 0 and REQUIRED_FIRST_ACTION is not None:
        return _filter_once([REQUIRED_FIRST_ACTION])

    if h:
        last = h[-1]
        if last in REQUIRE_NEXT:
            return _filter_once(REQUIRE_NEXT[last])

    return _filter_once(base)

def evaluate_len_shorter_better(state: SVAState) -> float:
    return -float(len(state.svas))

def expensive_eval_stub(state: SVAState, path: str) -> float:
    return 0.0

def get_unvisited_assertions(mcts: MCTS, init_state: SVAState) -> Dict[int, str]:
    if mcts.visit_tracker is None:
        raise RuntimeError("Visit Tracker 未初始化，请先执行 mcts.search()")
    
    unvisited_indices = mcts.visit_tracker.get_unvisited()
    
    unvisited_assertions = {}
    for idx in unvisited_indices:
        if idx in init_state.svas:
            unvisited_assertions[idx] = init_state.svas[idx]
    
    return unvisited_assertions


def get_visited_assertions(mcts: MCTS, init_state: SVAState) -> Dict[int, str]:
    if mcts.visit_tracker is None:
        raise RuntimeError("Visit Tracker 未初始化，请先执行 mcts.search()")
    
    visited_indices = mcts.visit_tracker.get_visited()
    
    visited_assertions = {}
    for idx in visited_indices:
        if idx in init_state.svas:
            visited_assertions[idx] = init_state.svas[idx]
    
    return visited_assertions


def get_visit_statistics(mcts: MCTS) -> Dict[str, int]:
    if mcts.visit_tracker is None:
        raise RuntimeError("Visit Tracker 未初始化，请先执行 mcts.search()")
    
    return mcts.visit_tracker.get_statistics()


def demo(path: str, svas: List[str]):
    init_svas = {i: sva for i, sva in enumerate(svas)}

    init_state = SVAState(init_svas)
    
    def expensive_eval_with_path(state: SVAState) -> float:
        return expensive_eval_stub(state, path)
    
    mcts = MCTS(
        apply_rule_fn=apply_rule,
        rule_selector_fn=rule_selector_with_constraints,
        evaluate_fn=evaluate_len_shorter_better,
        iteration_limit=300,
        c_param=1.4,
        seed=7,
        verbose=True,
        expensive_eval_fn=expensive_eval_with_path,  
        k_expensive=3,
        reward_distribution="uniform",
    )

    best_state = mcts.search(init_state)
    return best_state.svas
    

if __name__ == "__main__":
    prop_file = os.path.join(path, "property.txt")
    prop_file = ""
    with open(prop_file, "r", encoding="utf-8") as f:
        lines = [line.strip() for line in f if line.strip()]
    start_time = time.time()
    demo(path=path, svas=lines)
    end_start = time.time()
