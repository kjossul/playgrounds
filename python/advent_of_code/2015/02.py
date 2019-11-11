#! /usr/bin/env python3

with open("02.txt") as f:
    paper = 0
    ribbon = 0
    for line in f.read().splitlines():
        l, w, h = sorted(int(x) for x in line.split('x'))
        paper += 2*l*w + 2*l*h + 2*w*h + l*w
        ribbon += 2*l + 2*w + l*w*h
    print(f"Paper: {paper}, Ribbon: {ribbon}")
