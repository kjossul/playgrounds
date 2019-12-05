#! /usr/bin/env python3

def is_safe(x):
    return x in ((True, True, False), (False, True, True), (True, False, False), (False, False, True))


if __name__ == '__main__':
    s = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"
    N = 400000
    safe_tiles = 0
    rows = [(False,) + tuple(c == '^' for c in s) + (False,)]
    for i in range(N):
        safe_tiles += len(rows[i]) - 2 - sum(rows[i])
        rows.append((False,) + tuple(is_safe(rows[i][j - 1:j + 2]) for j in range(1, len(rows[i]) - 1)) + (False,))
    print(safe_tiles)
