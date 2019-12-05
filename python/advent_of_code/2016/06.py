#! /usr/bin/env python3

from collections import Counter

s1 = ''
s2 = ''

with open("06.txt") as f:
    for col in zip(*f.readlines()):
        chart = Counter(col).most_common()
        s1 += chart[0][0]
        s2 += chart[-1][0]

print(s1, s2)
