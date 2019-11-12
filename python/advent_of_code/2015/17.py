#! /usr/bin/env python3

from itertools import combinations

def solve(n, xs):
    cs = []
    for r in range(1, len(xs)+1):
        for c in combinations(xs, r):
            if sum(c) == n:
                cs.append(c)
    return cs


if __name__ == '__main__':
    with open("17.txt") as f:
        xs = list(map(int, f.read().splitlines()))
        ys = solve(150, xs)
        print(len(ys))
        print(len([y for y in ys if len(y) == len(ys[0])]))
