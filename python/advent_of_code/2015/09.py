#! /usr/bin/env python3

import re
from collections import defaultdict
from itertools import permutations

def solve(ls):
    """Travelling salesman problem"""
    dist = defaultdict(dict)
    p = re.compile("(\w+) to (\w+) = (\d+)")
    for l in ls:
        m = p.match(l)
        a, b, d = m.group(1), m.group(2), int(m.group(3))
        dist[a][b] = d
        dist[b][a] = d
    cities = dist.keys()
    m = M = None
    for cs in permutations(cities):
        length = sum(dist[cs[i]][cs[i+1]] for i in range(len(cs)-1))
        m = length if m is None else min(m, length)
        M = length if M is None else max(M, length)
    return m, M

if __name__ == '__main__':
    with open("09.txt") as f:
        ls = f.read().splitlines()
        x = solve(ls)
        print(x)

