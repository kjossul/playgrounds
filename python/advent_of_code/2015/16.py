#! /usr/bin/env python3

import re

def solve(ls):
    p = re.compile("\w+: \d+")
    clues = {
        "children": 3,
        "cats": 7,
        "samoyeds": 2,
        "pomeranians": 3,
        "akitas": 0,
        "vizslas": 0,
        "goldfish": 5,
        "trees": 3,
        "cars": 2,
        "perfumes": 1
    }
    sues = []
    for i, l in enumerate(ls, start=1):
        pairs = (x.split(': ') for x in p.findall(l))
        sues.append({v: int(k) for v,k in pairs})
        if all(get_op(k)(v, clues[k]) for k,v in sues[-1].items()):
            print(i)

def get_op(k):
    if k in ('cats', 'trees'):
        return lambda x,y: x > y
    elif k in ('pomeranians', 'goldfish'):
        return lambda x,y: x < y
    else:
        return lambda x,y: x == y

if __name__ == '__main__':
    with open("16.txt") as f:
        ls = f.read().splitlines()
        solve(ls)

