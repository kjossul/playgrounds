#! /usr/bin/env python3

import re
from collections import defaultdict
from itertools import permutations

def solve(ls, extra=[]):
    x = defaultdict(lambda: defaultdict(int))
    p = re.compile("(\w+) would (\w+) (\d+).*to (\w+)")
    for l in ls:
        m = p.match(l)
        a, s, v, b = m.groups()
        v = int(v) if s == 'gain' else -int(v)
        x[a][b] = v
    best = 0
    people = list(x.keys()) + extra
    for p in permutations(people):
        v = sum(x[p[i]][p[i+1]] + x[p[i+1]][p[i]] for i in range(len(p)-1)) + x[p[0]][p[-1]] + x[p[-1]][p[0]]
        best = max(best, v)
    return best

if __name__ == '__main__':
    with open("13.txt") as f:
        ls = f.read().splitlines()
        s1 = solve(ls)
        print(s1)
        s2 = solve(ls, ['Me'])
        print(s2)
