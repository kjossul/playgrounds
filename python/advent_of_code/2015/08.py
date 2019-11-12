#! /usr/bin/env python3

def solve(ls):
    t = 0
    for l in ls:
        escaped = bytes(l, "utf-8").decode("unicode_escape")
        t += len(l) - len(escaped) + 2
    return t

def part2(t):
    t = 0
    for l in ls:
        t += 2 + l.count('"') + l.count('\\')
    return t

if __name__ == '__main__':
    test = solve(["", "abc", "aaa\\aaa", "\\x27"])
    assert test == 12
    with open("08.txt") as f:
        ls = f.read().splitlines()
        print(solve(ls))
        print(part2(ls))
