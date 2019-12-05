#! /usr/bin/env python3

import re
from collections import Counter

s1 = 0

def caesar(string, shift):
    cipher = ''
    for char in string:
        if char == '-':
            cipher += ' '
        elif char.isupper():
            cipher += chr((ord(char) + shift - 65) % 26 + 65)
        else:
            cipher += chr((ord(char) + shift - 97) % 26 + 97)

    return cipher

with open("04.txt") as f:
    for name,sid,check in re.findall(r'(.*)-(\d+)\[(\w+)\]', f.read()):
        top = Counter(name.replace('-', '')).most_common()
        top.sort(key=lambda x: (-x[1], x[0]))
        if ''.join(x[0] for x in top[:5]) == check:
            s1 += int(sid)
        if caesar(name, int(sid)) == 'north pole object storage':
            print(int(sid))

print(s1)
