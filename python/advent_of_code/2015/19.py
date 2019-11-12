#! /usr/bin/env python3

import re

def solve(ls):
    subs = tuple(x.split(' => ') for x in ls[:-2])
    s = ls[-1]
    # part 1
    s1 = replace_all(s, subs)
    # part 2, DFS with cursor to avoid substitutions where string is correct
    i, frontier = 1, ['e']
    cursor = 0
    while True:
        new = set().union(*[replace_all(el, subs, cursor) for el in frontier])
        if s in new:
            return s1, i
        frontier = list(sorted(new, key=lambda x: compare(s, x), reverse=True))[:len(s)]
        i += 1
        cursor = max(0, compare(s, frontier[-1]) - 2)


def compare(s1, s2):
    i = 0
    while i < min(len(s1), len(s2)):
        if s1[i] != s2[i]:
            return i
        else:
            i += 1
    return i

def replace_all(s, subs, cursor=0):
    return {replaceN(s, old, new, n, cursor) for old,new in subs for n in range(1, s.count(old)+1)}

def replaceN(s, old, new, n, cursor=0):
    return s[:cursor] + s[cursor:].replace(old, '$', n-1).replace(old, new, 1).replace('$', old)


if __name__ == '__main__':
    with open("19.txt") as f:
        ls = f.read().splitlines()
        s1, s2 = solve(ls)
        print(len(s1), s2)

