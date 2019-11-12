#! /usr/bin/env python3

import re
from collections import defaultdict

def solve(ls, part2=False):
    gates = {}
    binary_op = {
        'AND': lambda x,y: x&y,
        'OR': lambda x,y: x|y,
        'LSHIFT': lambda x,y: x<<y,
        'RSHIFT': lambda x,y: x>>y
    }
    p = re.compile("(.*) -> (\w+)")
    while ls:
        l = ls.pop(0)
        m = p.match(l)
        left = m.group(1).split()
        right = m.group(2)
        if len(left) == 1:
            v = get_gate(gates, left[0])
            if v is None:
                ls.append(l)
            else:
                gates[right] = v
        elif len(left) == 2:
            v = get_gate(gates, left[1])
            if v is None:
                ls.append(l)
            else:
                gates[right] = ~v  # NOT instruction
        else:
            v1, v2 = get_gate(gates, left[0]), get_gate(gates, left[2])
            if v1 is None or v2 is None:
                ls.append(l)
            else:
                gates[right] = binary_op[left[1]](v1, v2)
        if gates.get('a'):
            return gates['a']

def get_gate(gates, item):
    if item.isnumeric():
        return int(item)
    else:
        return gates.get(item)

if __name__ == '__main__':
    with open("07.txt") as f:
        ls = f.read().splitlines()
        s1 = solve(ls)
        print(s1)
    # part 2
    with open("07.txt") as f:
        t = f.read()
        t = t.replace('1674', str(s1))
        print(solve(t.splitlines()))

