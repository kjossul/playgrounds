import re
from itertools import combinations
import pprint
from math import sqrt

from solver import Game

Nx = 34
Ny = 31
SIZES = [[None for _ in range(Nx)] for _ in range(Ny)]


def triangle_area(a, b, c):
    s = (a + b + c) / 2
    return sqrt(s * (s - a) * (s - b) * (s - c))


def distance(c1, c2):
    return sqrt((c1[0] - c2[0]) ** 2 + (c1[1] - c2[1]) ** 2)


def yield_coords():
    for x in range(Nx):
        for y in range(Ny):
            yield (y, x)


def is_viable(used, y1, x1, y2, x2):
    return all(0 <= x < Nx for x in (x1, x2)) and all(0 <= y < Ny for y in (y1, y2)) and used[y1][x1] != 0 and used[y1][
        x1] <= SIZES[y2][x2] - used[y2][x2]


def viable_pairs(used):
    for c1, c2 in combinations(yield_coords(), r=2):
        for a, b in ((c1, c2), (c2, c1)):
            if is_viable(used, *a, *b):
                yield (a, b)


def viable_neighbours(used, zero):
    for i, j in ((1, 0), (0, 1), (-1, 0), (0, -1)):
        other = (zero[0] + i, zero[1] + j)
        try:
            if is_viable(used, *other, *zero):
                yield (other, zero)
        except IndexError:
            pass


def expander(s, step):
    target, zero, used = s
    for source, dest in viable_neighbours(used, zero):
        new_target = target if source != target else dest
        updates = {source: 0, dest: used[source[0]][source[1]] + used[dest[0]][dest[1]]}
        new_used = tuple(tuple(updates.get((y, x), u) for x, u in enumerate(row)) for y, row in enumerate(used))
        yield ((new_target, source, new_used), step + 1)


def t(s):
    target, zero, used = s
    return target == (0, 0)


def h(s, step):
    target, zero, used = s
    return round(100 * distance((0, 0), target) + distance(zero, target))


def hashfun(s, step):
    target, zero, used = s
    return (target, zero)


if __name__ == '__main__':
    p = re.compile(r"(\d+)-y(\d+)\D*(\d+)\D*(\d+)\D*(\d+)\D*(\d+)")
    used = [[None for _ in range(Nx)] for _ in range(Ny)]

    with open("22.txt") as f:
        ls = f.readlines()
        for line in ls[2:]:
            m = p.search(line)
            data = tuple(map(int, m.groups()))
            x, y = data[:2]
            SIZES[y][x] = data[2]
            used[y][x] = data[3]

    viable = len(list(viable_pairs(used)))
    print(viable)
    target = (0, Nx - 1)
    zero = (25, 20)
    s0 = (target, zero, tuple(tuple(row) for row in used))
    game = Game(s0, expander, t, h=h, min_h=True, hashfun=hashfun)
    s2 = game.solve()
    print(s2[1])
