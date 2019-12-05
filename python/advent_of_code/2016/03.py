#! /usr/bin/env python3

with open("03.txt") as f:
    s1 = s2 = 0
    lines = []
    for line in f.readlines():
        xs = [int(n) for n in line.split()]
        lines.append(xs)
        xs = list(sorted(xs, reverse=True))
        if xs[0] < xs[1] + xs[2]:
            s1 += 1
    for xs in zip(*lines):
        for i in range(0, len(xs), 3):
            sizes = list(sorted(xs[i:i+3]))
            if sizes[0]+sizes[1] > sizes[2]:
                s2 += 1
    print(s1)
    print(s2)

