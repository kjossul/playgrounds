#! /usr/bin/env python3

import re
from collections import defaultdict
from itertools import product
from PIL import Image

def solve(ls):
    lights = set()
    brightness = defaultdict(int)

    p = re.compile('(\D+) (\d+),(\d+) .+ (\d+),(\d+)')
    for l in ls:
        m = p.match(l)
        action = m.group(1)
        x1, y1, x2, y2 = [int(g) for g in m.group(2, 3, 4, 5)]
        for light in product(range(x1, x2 + 1), range(y1, y2 + 1)):
            # part 1
            if action == 'turn on' or (action == 'toggle' and light not in lights):
                lights.add(light)
            else:
                lights.discard(light)
            # part 2
            if action == 'turn off':
                brightness[light] = max(0, brightness[light] - 1)
            else:
                brightness[light] += 1 if action == 'turn on' else 2
    return len(lights), sum(brightness.values())

def draw(lights):
    img = Image.new('RGB', (1000, 1000), 'white')
    for light in lights:
        img.putpixel(light, (0, 0, 0))
    img.show()

if __name__ == '__main__':
    # assert solve1(["turn on 0,0 through 999,999"]) == 1e6
    # assert solve1(["turn on 0,0 through 999,999", "toggle 0,0 through 999,0"]) == 1e6 - 1000

    with open("06.txt") as f:
        ls = f.read().splitlines()
        print(solve(ls))
