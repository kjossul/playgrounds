#! /usr/bin/env python3

with open("03.txt") as f:
    houses = {(0, 0)}
    s = [0, 0]
    # (element to modify, value to add)
    a = {
            'v': (1, -1),
            '^': (1, 1),
            '<': (0, -1),
            '>': (0, 1)
        }
    for c in f.read():
        s[a[c][0]] += a[c][1]
        houses.add(tuple(s))
    print(len(houses))

