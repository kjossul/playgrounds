#! /usr/bin/env python3

from collections import defaultdict

def solve(ls, n):
    m = {i: {j: c == '#' for j,c in enumerate(row)} for i, row in enumerate(ls)}
    for _ in range(n):
        fix_corners(m)
        update(m)
    fix_corners(m)
    return sum(sum(m[c][r] for c in m[r]) for r in m)

def neighbours(m):
    out = defaultdict(dict)
    for r, row in enumerate(m):
        for c in range(len(m[r])):
            out[r][c] = sum(m.get(r+i, {}).get(c+j, 0) for i in (-1, 0, 1) for j in (-1, 0, 1)) - m[r][c]
    return out

def update(m):
    n = neighbours(m)
    for i, row in enumerate(ls):
        for j, v in m[i].items():
            if v:
                m[i][j] = 2 <= n[i][j] <= 3
            else:
                m[i][j] = n[i][j] == 3

def fix_corners(m):
    m[0][0] = m[0][99] = m[99][0] = m[99][99] = True

if __name__ == '__main__':
    with open("18.txt") as f:
        ls = f.read().splitlines()
        s1 = solve(ls, 100)
        print(s1)
