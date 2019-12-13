from collections import defaultdict
from fractions import Fraction
from itertools import cycle, islice

HORIZONTAL = {-1: "W", 0: "", 1: "E"}
VERTICAL = {-1: "S", 0: "", 1: "N"}


def compare(a, b):
    if a < b:
        return -1
    elif a == b:
        return 0
    else:
        return 1


def distance(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)


def yield_asteroids(station, navigator):
    directions = ("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    for direction in cycle(directions):
        for line in sorted(navigator[station][direction], reverse=True):
            asteroids = navigator[station][direction][line]
            target = min(asteroids, key=lambda a: distance(*station, *a))
            yield target
            asteroids.remove(target)


if __name__ == '__main__':
    asteroids = set()
    with open("10.txt") as f:
        for y, line in enumerate(f.readlines()):
            for x, c in enumerate(line):
                if c == '#':
                    asteroids.add((x, y))

    d = {}
    navigator = defaultdict(lambda: defaultdict(lambda: defaultdict(set)))
    for x1, y1 in asteroids:
        asteroid = x1, y1
        d[asteroid] = set()
        for x2, y2 in asteroids - {asteroid}:
            sector = VERTICAL[compare(y1, y2)] + HORIZONTAL[compare(x2, x1)]
            try:
                line = Fraction(y1 - y2, x2 - x1)
            except ZeroDivisionError:
                line = 0
            d[asteroid].add((sector, line))
            navigator[(x1, y1)][sector][line].add((x2, y2))
    s1 = max(d, key=lambda x: len(d[x]))
    print(s1, len(d[s1]))
    s2 = next(islice(yield_asteroids(s1, navigator), 199, None))
    print(s2[0] * 100 + s2[1])
