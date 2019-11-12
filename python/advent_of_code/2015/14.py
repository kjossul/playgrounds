#! /usr/bin/env python3

import re

class Deer:
    def __init__(self, name, v, t1, t2):
        self.name = name
        self.v = v
        self.t1 = t1
        self.t2 = t2
        self.resting = False
        self.i = 0
        self.distance = 0
        self.points = 0

    def step(self):
        t = self.t1 if not self.resting else self.t2
        if self.i >= t:  # toggle rest / fly
            self.resting = not self.resting
            self.i = 0
        self.i += 1
        if not self.resting:
            self.distance += self.v

    def __str__(self):
        return f"{self.name:10}: {self.distance:4}km, {'Resting' if self.resting else ' Flying'}, {self.points:4}pts."

    def __repr__(self):
        return str(self)

def solve(ls, t):
    deers = []
    p = re.compile("(\w+)\D*(\d+)\D*(\d+)\D*(\d+).*\.")
    for l in ls:
        m = p.match(l)
        deers.append(Deer(m.group(1), *map(int, m.groups()[1:])))
    for i in range(t):
        best = 0
        for d in deers:
            d.step()
            best = max(best, d.distance)
        for d in deers:
            if d.distance == best:
                d.points += 1
    return deers


if __name__ == '__main__':
    with open("14.txt") as f:
        ls = f.read().splitlines()
        deers = solve(ls, 2503)
        s1 = max(d.distance for d in deers)
        s2 = max(d.points for d in deers)
        print(s1, s2)

