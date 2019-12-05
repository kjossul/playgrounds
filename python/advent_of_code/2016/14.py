#! /usr/bin/env python3

import re
from hashlib import md5


def solve(stretching=1):
    global keys, hashes
    keys = []
    hashes = []
    salt = "ihaygndm"
    h = md5()
    i = 0
    p = re.compile(r'(\w)\1{2}')
    while len(keys) < 64:
        s = salt + str(i)
        for _ in range(stretching):
            hc = h.copy()
            hc.update(s.encode())
            s = hc.hexdigest()
        hashes.append(s)
        if i >= 1000:
            j = i - 1000
            m = p.search(hashes[j])
            if m:
                is_valid = any(m.group(1) * 5 in hash for hash in hashes[j + 1:i + 1])
                if is_valid:
                    keys.append(hashes[j])
        i += 1
    return hashes.index(keys[-1])


if __name__ == '__main__':
    s1 = solve()
    s2 = solve(2017)
    print(s1, s2)
