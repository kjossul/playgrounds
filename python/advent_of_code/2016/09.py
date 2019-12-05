#! /usr/bin/env python3

import re

p = re.compile(r'\((\d+)x(\d+)\)')

def get_len(length, mul, s):
    m = p.search(s)
    if m:
        length, inner = map(int, m.groups())
        token = s[m.end():m.end()+length]
        rest = s[m.end()+length:]
        return m.start() * mul + get_len(length, mul*inner, token) + get_len(len(rest), mul, rest)
    else:
        return length*mul

with open("09.txt") as f:
    t = f.read().strip()
    out = ''
    s2 = 0
    while True:
        m = p.search(t)
        if not m:
            out += t
            s2 += len(t)
            break
        i, j = map(int, m.groups())
        token = t[m.end():m.end()+i]
        out += t[:m.start()] + token * j
        s2 += get_len(i, j, token)
        t = t[m.end()+i:]

print(len(out.strip()), s2)
        
