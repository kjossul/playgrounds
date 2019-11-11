#!/usr/bin/env python

with open("01.txt") as f:
    t = f.read()
    u = t.count('(')
    d = t.count(')')
    print(u - d)
