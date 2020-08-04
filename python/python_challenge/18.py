#! /usr/bin/env python3

import re
from difflib import ndiff

encoding = 'ISO-8859-1'

with open("18_deltas.txt") as f:
    t = f.read()
    r = r"\s{3}(\w)"
    fixed = re.sub(r, r'*\1', t)
    lines = ([], [])
    for line in fixed.splitlines():
        spl = line.split('*')
        for i, x in enumerate(spl):
            if x.strip():
                lines[i].append(x.strip())

    out = [b'', b'', b'']
    d = {' ': 0, '+': 1, '-': 2}
    for x in ndiff(*lines):
        encoded = b''.join(chr(int(y, base=16)).encode(encoding) for y in x[2:].split())
        out[d[x[0]]] += encoded
    for i in range(3):
        with open(f"18_diff{i}.png", 'wb') as f:
            f.write(out[i]) 

    # ../hex/bin.html butter fly

