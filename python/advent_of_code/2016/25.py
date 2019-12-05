#! /usr/bin/env python3

from functools import partial
from collections import defaultdict
from math import factorial

registers = defaultdict(int)
code = []
last = 1


class BadSignal(Exception):
    pass


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


def out(r, pc):
    global last
    if registers[r] == last:
        raise BadSignal
    last = registers[r]
    return pc + 1


if __name__ == '__main__':
    with open("25.txt") as f:
        for line in f.readlines():
            code.append(line.split())

        pc = 0
        i = 0
        while pc < len(code):
            args = code[pc]
            try:
                pc = globals()[args[0]](*args[1:], pc)
            except ValueError:
                pc += 1
            except BadSignal:
                pc = 0
                registers.clear()
                print(f"{i} is not right.")
                i += 1
                registers['a'] = i
                last = 1

        print(registers)
