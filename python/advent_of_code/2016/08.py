#! /usr/bin/env python3

import re
from collections import deque

w, h = 50, 6
screen = [[False for x in range(w)] for y in range(h)]

p1 = re.compile(r'(\d+)x(\d+)')
p2 = re.compile(r'(\w)=(\d+)\D*(\d+)')

def print_screen(screen):
    for row in screen:
        print(''.join('#' if led else '.' for led in row))

with open("08.txt") as f:
    for line in f.readlines():
        m1 = p1.search(line)
        if m1:
            for y in range(int(m1.group(1))):
                for x in range(int(m1.group(2))):
                    screen[x][y] = True
        else:
            m2 = p2.search(line)
            c = m2.group(1)
            i = int(m2.group(2))
            r = int(m2.group(3))
            if c == 'x':
                screen = list(list(x) for x in zip(*screen))
            d = deque(screen[i])
            d.rotate(r)
            screen[i] = list(d)
            if c == 'x':
                screen = list(list(x) for x in zip(*screen))

print(sum(led for row in screen for led in row))
print_screen(screen)
