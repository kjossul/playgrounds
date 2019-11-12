#! /usr/bin/env python3

import json

def parse(x, part2=False):
    if isinstance(x, list):
        return sum(parse(y, part2) for y in x)
    elif isinstance(x, dict):
        if not part2 or 'red' not in x.values():
            return sum(parse(y, part2) for y in x.values())
        else:
            return 0
    else:
        return x if isinstance(x, int) else 0


if __name__ == '__main__':
    with open("12.txt") as f:
        x = json.load(f)
        s1 = parse(x)
        s2 = parse(x, True)
        print(s1, s2)

