#! /usr/bin/env python3

import re

def add_coords(*args):
    return tuple(a+b for a,b in zip(*args))

pos = [(0,0)]
steps = ((0, 1), (1, 0), (0, -1), (-1, 0))
direction = 0
change = {'L': -1, 'R': 1}
s2 = None
with open("01.txt") as f:
    for c,v in re.findall(r'(\w)(\d+)', f.read()):
        direction = (direction + change[c]) % 4
        v = int(v)
        step = steps[direction]
        for i in range(1, v+1):
            s1 = add_coords(pos[-1], step)
            if s2 is None and s1 in pos:
                s2 = s1
            pos.append(s1)

for s in (s1, s2):
    print(abs(s[0]) + abs(s[1]))

