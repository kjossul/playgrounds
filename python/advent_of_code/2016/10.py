#! /usr/bin/env python3

import re
import operator
from functools import reduce

class Bot:
    def __init__(self, n):
        self.n = n
        self.targets = [None] * 2
        self.low = self.high = None

    def take(self, n):
        if self.low is None:
            self.low = n
        else:
            self.high = n
            if self.low > self.high:
                self.low, self.high = self.high, self.low
            self.give()
    
    def give(self):
        if None in [self.high, self.low] + self.targets:
            return
        self.targets[0].take(self.low)
        self.targets[1].take(self.high)

    def __str__(self):
        return str(f"{self.n}: ({self.low}, {self.high})")

    def __repr__(self):
        return str(self)


with open("10.txt") as f:
    bots = {}
    outputs = {}
    p1 = re.compile("(\d+).* (\w+) (\d+).* (\w+) (\d+)")
    p2 = re.compile("(\d+) goes to bot (\d+)")
    for line in f.readlines():
        m = p1.search(line)
        if m:
            n = int(m.group(1))
            bot = bots.setdefault(n, Bot(n))
            targets = []
            for i in (2, 4):
                v = int(m.group(i+1))
                d = bots if m.group(i) == 'bot' else outputs
                targets.append(d.setdefault(v, Bot(v)))
            bot.targets = targets
            bot.give()
        else:
            v, n = map(int, p2.search(line).groups())
            bots.setdefault(n, Bot(n)).take(v)

    print([(bot) for n,bot in bots.items() if bot.low == 17 and bot.high == 61])
    print(reduce(operator.mul, [outputs[i].low for i in range(3)], 1))
