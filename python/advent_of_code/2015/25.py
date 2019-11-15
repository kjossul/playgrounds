#! /usr/bin/env python3

def solve(s, m, d):
    i = j = 1
    n = 1
    while i != 2947 or j != 3029:
        i -= 1
        j += 1
        if i == 0:
            n += 1
            i = n
            j = 1
        s = s * m % d
    print(i, j, n, s)
    return s

if __name__ == '__main__':
    s, m, d = 20151125, 252533, 33554393
    s1 = solve(s, m, d)
    print(s1)
