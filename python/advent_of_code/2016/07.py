#! /usr/bin/env python3

import re
from itertools import product

# Matches abba sequences
p1 = re.compile(r'(\w)(?!\1)(\w)\2\1')
# Matches abba sequences inside square brackets
p2 = re.compile(r'\[[^\]]*(\w)(?!\1)(\w)\2\1[^\]]*\]')
# Matches aba sequences with overlapping 
p3 = re.compile(r'(?=((\w)(?!\2)\w\2))')

s1 = 0
s2 = 0
with open("07.txt") as f:
    for line in f.readlines():
        if p1.search(line) and not p2.search(line):
            s1 += 1
        # Finds positions of [..] blocks
        brs = {(m.start(), m.end()) for m in re.finditer(r'\[[^\[]*\]', line)}
        for m1, m2 in product(p3.finditer(line), repeat=2):
            bab, aba = m1.group(1), m2.group(1)
            if not any(s < m1.start(1) < e for s,e in brs) and any(s < m2.start(1) < e for s,e in brs) and bab[1:] + bab[1] == aba:
                s2 += 1
                break

print(s1, s2)

