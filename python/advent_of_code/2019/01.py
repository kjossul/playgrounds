import math


def req(mass):
    return math.floor(mass // 3) - 2


def solve2(masses):
    s2 = 0
    for mass in masses:
        mass = req(mass)
        while mass > 0:
            s2 += mass
            mass = req(mass)
    return s2


if __name__ == '__main__':
    with open("01.txt") as f:
        masses = tuple(int(mass) for mass in f.readlines())
        print(sum(req(mass) for mass in masses))
        print(solve2(masses))
