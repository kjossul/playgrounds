#! /usr/bin/env python3

import re

def step(s):
    a = s
    b = ''.join('0' if c == '1' else '1' for c in a[::-1])
    return a + '0' + b


def checksum(s):
    p = re.compile(r'\w\w')
    out = ''
    for a,b in p.findall(s):
        if a == b:
            out += '1'
        else:
            out += '0'
    if len(out) % 2 == 0:
        return checksum(out)
    else:
        return out

if __name__ == '__main__':
    l = 35651584
    s = "10001001100000001"
    while len(s) < l:
        s = step(s)
    s1 = checksum(s[:l])
    print(s1)