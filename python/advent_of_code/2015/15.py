#! /usr/bin/env python3

import re
import operator
from functools import reduce

def solve(ls):
    d = []
    p = re.compile("-*\d+")
    for l in ls:
        d.append([int(x) for x in p.findall(l)])
    d = list(map(list, zip(*d)))
    best_tuple, best_score = None, 0
    for t in gen_tuples():
        s = score(d, t)
        is500cal = sum(a*b for a,b in zip(d[-1], t)) == 500  # set to True for first solution
        if s > best_score and is500cal:
            best_tuple = t
            best_score = s
    return best_score, best_tuple

def score(m, t):
    terms = (max(sum(a*b for a,b in zip(el, t)), 0) for el in m[:-1])
    return reduce(operator.mul, terms, 1)

def gen_tuples():
    for x1 in range(101):
        for x2 in range(101-x1):
            for x3 in range(101-(x1+x2)):
                yield(x1, x2, x3, 100 - (x1+x2+x3))

if __name__ == '__main__':
    with open("15.txt") as f:
        ls = f.read().splitlines()
        s1 = solve(ls)
        print(s1)

