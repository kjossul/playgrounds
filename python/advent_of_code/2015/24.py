#! /usr/bin/env python3

from itertools import combinations, permutations
from collections import defaultdict
from functools import reduce
import operator

def solve(xs, part2=False):
    avg = sum(xs) // (3 if not part2 else 4)
    gs = set()
    for i in range(len(xs) // 2):
        if gs:
            break
        for c in combinations(xs, i):
            if sum(c) == avg:
                gs.add(tuple(c))
    for x in sorted(gs, key=lambda x: reduce(operator.mul, x, 1)):
        rest = tuple(n for n in xs if n not in x)
        for i in range(len(rest) // 2):
            for y in combinations(rest, i):
                rest2 = tuple(n for n in xs if n not in x+y)
                if not part2:
                    z = rest2
                    if sum(y) == sum(z) == avg:
                        return (x, y, z)
                elif sum(y) == avg:
                    for j in range(len(rest2) // 2):
                        for z in combinations(rest2, j):
                            w = tuple(n for n in xs if n not in x+y+z)
                            if sum(z) == sum(w) == avg:
                                return (x, y, z, w)

def memoize(f):
    d = {}
    def _f(*args):
        try:
            return d[args]
        except KeyError:
            d[args] = f(*args)
            return d[args]
    return _f

def check_groups(xs):
    return len({sum(x) for x in xs}) is 1

if __name__ == '__main__':
    with open("24.txt") as f:
        xs = [int(x) for x in f.read().splitlines()]
        s1 = solve(xs, True)
        print(s1, reduce(operator.mul, s1[0], 1))

