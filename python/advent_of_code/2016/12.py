#! /usr/bin/env python3

from functools import partial
from collections import defaultdict

registers = defaultdict(int)


def inc(register, pc):
    registers[register] += 1
    return pc + 1


def dec(register, pc):
    registers[register] -= 1
    return pc + 1


def cpy(v, register, pc):
    if v.isnumeric():
        registers[register] = int(v)
    else:
        registers[register] = registers[v]
    return pc + 1


def jnz(v, n, pc):
    if v.isnumeric():
        x = int(v)
    else:
        x = registers[v]
    if x != 0:
        return pc + int(n)
    else:
        return pc + 1


if __name__ == '__main__':
    with open("12.txt") as f:
        pc = 0
        code = []
        registers['c'] = 1
        for line in f.readlines():
            args = line.split()
            code.append(partial(globals()[args[0]], *args[1:]))

        while pc < len(code):
            pc = code[pc](pc)

        print(registers)
