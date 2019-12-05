#! /usr/bin/env python3

import hashlib

s = 'reyedfim'
i = 0
s1 = ''
s2 = [None] * 8
while len(s1) < 8 or None in s2:
    m = hashlib.md5((s + str(i)).encode()).hexdigest()
    if set(m[:5]) == {'0'}:
        s1 += m[5]
        if '0' <= m[5] <= '7' and s2[int(m[5])] is None:
            s2[int(m[5])] = m[6]
    i += 1

print(s1[:8])
print(''.join(s2))
