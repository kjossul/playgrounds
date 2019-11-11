#! /usr/bin/env python

with open("01.txt") as f:
    c = 1
    fl = 0
    while True:
        ch = f.read(1)
        if not ch:
            break
        fl += 1 if ch == '(' else -1
        if fl < 0:
            print(c)
            break
        c += 1
