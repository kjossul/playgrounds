#! /usr/bin/env python3
import hashlib

def solve(k, length):
    m = hashlib.md5()
    m.update(k.encode('utf-8'))
    n = 0
    while True:
        h = m.copy()
        h.update(str(n).encode('utf-8'))
        if set(h.hexdigest()[:length]) == {'0'}:
            return n
        n += 1

if __name__ == '__main__':
    # assert solve("abcdef") == 609043

    print(solve("iwrupvqb", 6))
