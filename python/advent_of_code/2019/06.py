from collections import defaultdict


def parse(ls, directs, indirects):
    for line in ls:
        a, b = line.strip().split(')')
        directs[b].add(a)
        for o in directs[a] | indirects[a]:
            indirects[b].add(o)


def path(a, b, directs):
    visited = [set(directs[a]), set(directs[b])]
    checked = defaultdict(int)
    curr = [directs[a].pop(), directs[b].pop()]
    i = 0
    while not set.intersection(*visited):
        for j in range(2):
            planet = curr[j]
            try:
                dest = directs[planet].pop()
                visited[j].add(dest)
                checked[planet] += i
                curr[j] = dest
            except KeyError:
                pass
        i += 1
    return i + checked[set.intersection(*visited).pop()]


if __name__ == '__main__':
    directs = defaultdict(set)
    indirects = defaultdict(set)
    with open("06.txt") as f:
        ls = f.readlines()
    old = None
    while True:
        parse(ls, directs, indirects)
        a = sum(len(x) for x in indirects.values())
        b = sum(len(x) for x in directs.values())
        if old is not None and old == a:
            break
        old = a
    print(a + b)
    print(path('YOU', 'SAN', directs))
