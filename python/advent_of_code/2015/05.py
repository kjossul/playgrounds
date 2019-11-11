#! /usr/bin/env python3

import string
from itertools import product

def solve(ws, f):
    return sum([f(w) for w in ws])

def is_nice1(s):
    a = sum(1 for c in s if c in 'aeiou') >= 3
    b = any(c*2 in s for c in string.ascii_lowercase)
    c = all(r not in s for r in ('ab', 'cd', 'pq', 'xy'))
    return a and b and c


def is_nice2(s):
    a = any(s.count(''.join(x)) > 1 for x in product(string.ascii_lowercase, repeat=2))
    b = any(str(x[1]) + ''.join(x) in s for x in product(string.ascii_lowercase, repeat=2))
    return a and b

if __name__ == '__main__':
    assert solve(['ugknbfddgicrmopn'], is_nice1) == 1
    assert solve(['uurcxstgmygtbstg'], is_nice2) == 0

    with open("05.txt") as f:
        ws = f.read().splitlines()
        print(solve(ws, is_nice1))
        print(solve(ws, is_nice2))
