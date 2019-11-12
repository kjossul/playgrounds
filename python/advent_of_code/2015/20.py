#! /usr/bin/env python3

import math

def find_power(n, div=2, mul=10, lim=None):
    i = 1
    house = 1
    presents = mul
    while presents < n:
        i += 1
        if lim and i > lim:
            return None
        house *= div
        presents += house * mul
    return house // div

def solve(n, div=2, mul=10, lim=None):
    house = find_power(n, div, mul=mul, lim=lim)
    print(f"Solving {n} for div={div}, mul={mul}, lim={lim}. Start={house}")
    for i in range(house, house*div + 1, div):
        p = sum(j * mul for j in find_divisors(i, lim=lim))
        if p >= n:
            return i

def find_divisors(n, lim=None):
    for i in range(1, math.ceil(n / 2)+1):
        q, r = divmod(n, i)
        if r == 0 and (not lim or q <= lim):
            yield i
    if n > 1:
        yield n

if __name__ == '__main__':
    assert list(find_divisors(10)) == [1, 2, 5, 10]
    n = 36000000
    d1 = min((2, 3, 5, 7, 11), key=lambda x: find_power(n, div=x))
    # s1 = solve(n, div=d1)
    # print(s1)
    s2 = solve(n, div=7, mul=11, lim=50)
    print(s2)

