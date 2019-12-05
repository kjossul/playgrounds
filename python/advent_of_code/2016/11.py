#! /usr/bin/env python3

# cobalt, polonium, promethium, ruthenium, thulium
# first 5 generators, second 5 microchips

from itertools import combinations
from collections import defaultdict
from copy import deepcopy


N = 7  # number of distinct items

def state_hash(e, s):
    h = str(e)
    for xs in s:
        x = y = z = 0
        for g,c in zip(xs[:N], xs[N:]):
            x += g
            y += c
            z += g and c
        h += f"_{x}_{y}_{z}_"
    return h

def is_valid_state(s):
    return all(is_valid_row(xs) for xs in s)

def is_valid_row(xs):
    has_gens = any(xs[:N])
    for g,c in zip(xs[:N], xs[N:]):
        if has_gens and c and not g:
            return False
    return True

def is_completed(s):
    return all(s[3])

def h(e, s):
    return sum(i*sum(s[i]) for i in range(4))*10 + e

def get_moves(e, s):
    for new_e in (e+i for i in (1, -1) if 0 <= e+i <= 3):
        for r in (1,2):
            for combo in combinations(range(N*2), r):
                if not all(s[e][col] for col in combo):
                    continue
                new_s = deepcopy(s)
                for col in combo:
                    new_s[e][col], new_s[new_e][col] = new_s[new_e][col], new_s[e][col]
                if is_valid_state(new_s):
                    yield (new_e, new_s)

s0 = [[False] * 10, 
        [False] * 10, 
        [False] * 5 + [False, True, True, False, False], 
        [True] * 5 +  [True, False, False, True, True]][::-1]

s1 = [[False] * 14,
        [False] * 14,
        [False] * 12 + [True, True],
        [True] * 12 + [False, False]][::-1]

d = defaultdict(list, {h(0, s1): [(0,s1, 0)]})
visited = set()
while True:
    frontier = d[max(d)]
    try:
        e, s, c = frontier.pop(0)
    except IndexError:
        del d[max(d)]
        continue
    sh = state_hash(e, s)
    if sh in visited:
        continue
    else:
        visited.add(sh)
    if is_completed(s):
        print(c)
        break
    for e, s in get_moves(e, s):
        d[h(e, s)].append((e,s, c+1))
        
