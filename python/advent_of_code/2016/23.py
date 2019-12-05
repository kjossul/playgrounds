#! /usr/bin/env python3

from functools import partial
from collections import defaultdict
from math import factorial

registers = defaultdict(int)
code = []


def get_value(v):
    try:
        return int(v)
    except ValueError:
        return registers[v]


def inc(register, pc):
    registers[register] += 1
    return pc + 1


def dec(register, pc):
    registers[register] -= 1
    return pc + 1


def cpy(v, register, pc):
    registers[register] = get_value(v)
    return pc + 1


def jnz(v, n, pc):
    x = get_value(v)
    if x != 0:
        return pc + get_value(n)
    else:
        return pc + 1


def tgl(v, pc):
    d = {'inc': 'dec', 'dec': 'inc', 'tgl': 'inc', 'jnz': 'cpy', 'cpy': 'jnz'}
    x = pc + get_value(v)
    try:
        code[x][0] = d[code[x][0]]
        print(registers)
    except IndexError:
        print(registers)
    return pc + 1


if __name__ == '__main__':
    with open("23.txt") as f:
        pc = 0
        registers['a'] = 7
        for line in f.readlines():
            code.append(line.split())

        while pc < len(code):
            args = code[pc]
            try:
                pc = globals()[args[0]](*args[1:], pc)
            except ValueError:
                pc += 1

        print(registers)

    # The whole code can be summerized as:
    print(factorial(12) + 73 * 95)
