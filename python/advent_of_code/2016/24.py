from collections import defaultdict
from itertools import combinations, permutations

from solver import Game

d = defaultdict(lambda: defaultdict(lambda: False))
flags = [None for _ in range(8)]
curr_target = None


def explorer(s):
    for c in ((1, 0), (0, 1), (-1, 0), (0, -1)):
        x, y = (s[0] + c[0], s[1] + c[1])
        if d[x][y]:
            yield (x, y)


def t(s):
    return s == curr_target


if __name__ == '__main__':
    with open("24.txt") as f:
        ls = f.readlines()
        for y, line in enumerate(ls[1:-1]):
            for x, c in enumerate(line[1:-1]):
                d[x][y] = c != '#'
                if c.isnumeric():
                    n = int(c)
                    flags[n] = (x, y)

    minpaths = {}
    for fs, fe in combinations(range(8), r=2):
        start, end = flags[fs], flags[fe]
        curr_target = end
        g = Game(start, explorer, t)
        s = g.solve()
        minpaths[(start, end)] = minpaths[(end, start)] = s[1]
    s1 = None
    for path in permutations(flags[1:]):
        l = sum(minpaths[(s, e)] for s, e in zip((flags[0],) + path, path))
        l += minpaths[(path[-1], flags[0])]
        s1 = l if s1 is None else min(s1, l)
    print(s1)
