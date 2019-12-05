from collections import deque
import re

from itertools import permutations


def swap_pos(s, x, y):
    t = list(s)
    x, y = int(x), int(y)
    t[x], t[y] = t[y], t[x]
    return ''.join(t)


def swap_letters(s, x, y):
    out = ''
    for c in s:
        if c == x:
            out += y
        elif c == y:
            out += x
        else:
            out += c
    return out


def rotate1(s, direction, n):
    d = deque(s)
    n = int(n)
    d.rotate(n * (-1 if direction == 'left' else 1))
    return ''.join(d)


def rotate2(s, c):
    d = deque(s)
    i = d.index(c)
    d.rotate(1 + i + (1 if i >= 4 else 0))
    return ''.join(d)


def reverse(s, x, y):
    x, y = int(x), int(y)
    middle = s[x:y + 1]
    return s[:x] + middle[::-1] + s[y + 1:]


def insert(s, x, y):
    x, y = int(x), int(y)
    l = list(s)
    c = l.pop(x)
    l.insert(y, c)
    return ''.join(l)


def execute(line, s, ps, fs):
    for p, f in zip(ps, fs):
        m = p.search(line)
        if m:
            return f(s, *m.groups())


if __name__ == '__main__':
    s1 = "abcdefgh"
    s2 = "fbgdceah"

    ps = [re.compile(s) for s in (r'(\d+) with position (\d+)', r'(\w) with letter (\w)', r'rotate (\w+) (\d+) step',
                                  r'of letter (\w)', r'(\d+) through (\d+)', r'(\d+) to position (\d+)')]
    fs = [swap_pos, swap_letters, rotate1, rotate2, reverse, insert]
    with open("21.txt") as f:
        ls = f.readlines()
    # part 1
    for line in ls:
        s1 = execute(line, s1, ps, fs)
    # part 2
    for p in permutations("abcdefgh"):
        s = ''.join(p)
        for line in ls:
            s = execute(line, s, ps, fs)
        if s == s2:
            break

    print(s1, ''.join(p))
