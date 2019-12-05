from collections import OrderedDict

from solver import Game
from hashlib import md5

N = 4

s0 = (0, 0)
d = (3, 3)
CODE = "veumntbg"


def f(s, step):
    h = md5((CODE + step).encode()).hexdigest()
    MOVES = OrderedDict({'U': (0, -1), 'D': (0, 1), 'L': (-1, 0), 'R': (1, 0)})
    for d, c in zip(MOVES.items(), h):
        direction, move = d
        new_s = (s[0] + move[0], s[1] + move[1])
        if all(0 <= n < N for n in new_s) and 'b' <= c <= 'f':
            yield (new_s, step + direction)


def t(s):
    return s == d


def hashfun(s, step):
    return str(s) + step


def h(s, step):
    return -len(step)


def h1(s, step):
    return abs(d[0] - s[0]) + abs(d[1] - s[1]) + len(step) * 10


if __name__ == '__main__':
    game1 = Game(s0, f, t, step='', h=h, hashfun=hashfun)
    print(game1.solve()[1])
    game2 = Game(s0, f, t, step='', h=h1, hashfun=hashfun, stop=False)
    longest = ''
    for s, steps in game2.solve():
        if len(steps) > len(longest):
            longest = steps
    print(len(longest))
