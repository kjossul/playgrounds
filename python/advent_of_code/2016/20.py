#! /usr/bin/env python3

from bisect import insort


def insert(i, xs, *args):
    valids = [x for x in args if x[0] <= x[1]]
    return xs[:i] + valids + xs[i + 1:], len(valids) - 1


if __name__ == '__main__':
    allowed = [(0, 4294967295)]
    with open("20.txt") as f:
        for line in f.readlines():
            a, b = map(int, line.strip().split('-'))
            delta = 0
            for i, pair in enumerate(allowed):
                x, y = pair
                if not (b < x or a > y):
                    allowed, n = insert(i + delta, allowed, (x, a - 1), (b + 1, y))
                    delta += n

    print(allowed[0][0])
    print(sum(b-a+1 for a,b in allowed))
