from math import ceil


def solve1(n):
    elves = list(range(1, n + 1))
    while len(elves) > 1:
        if len(elves) % 2 == 0:
            elves = elves[::2]
        else:
            elves = [elves[-1]] + elves[:-1:2]
    return elves


def solve2(n):
    elves = list(range(1, n + 1))
    l = len(elves)
    while l > 1:
        l = len(elves)
        start = l // 2 + (2 if l % 2 == 0 else 1)
        saved = [elves[i] for i in range(start, l, 3)]
        x = ceil(l / 3)
        elves = elves[x:l//2] + saved + elves[:x]
    return elves



if __name__ == '__main__':
    N = 3005290
    s1 = solve1(N)
    s2 = solve2(N)
    print(s1, s2)
