from itertools import product, chain

from solver import Game

MAGIC = 1358
s0 = (1, 1)
d = (31, 39)


def is_wall(x, y):
    if x < 0 or y < 0:
        return True
    a = x * x + 3 * x + 2 * x * y + y + y * y + MAGIC
    binary = f"{a:b}"
    return binary.count('1') % 2 != 0


def f(state, step):
    x, y = state
    return tuple(((x + i, y + j), step+1) for i, j in ((1, 0), (0, 1), (-1, 0), (0, -1)) if not is_wall(x + i, y + j))


def h(state, step):
    x, y = state
    return -(abs(x - d[0]) + abs(y - d[1]))


def t(state):
    return state == d


def print_track(coords):
    edge = max(chain.from_iterable(coords)) + 1
    map = ''
    for y in range(edge):
        for x in range(edge):
            if is_wall(x, y):
                map += ':'
            elif (x, y) in coords:
                map += 'O'
            else:
                map += ' '
        map += '\n'
    print(map)


if __name__ == '__main__':
    game1 = Game(s0, f, t, h=h)
    s, step = game1.solve()
    print_track(s)
    print(len(s) - 1)
    game2 = Game(s0, f, t, step_limit=50)
    game2.solve()
    print(len(game2.visited))
