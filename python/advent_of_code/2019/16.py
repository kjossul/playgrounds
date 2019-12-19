import datetime
from itertools import cycle, islice, tee


def fft(ns):
    base = [0, 1, 0, -1]
    l = len(ns)
    out = [0] * l
    for i in range(l):
        for n, m in zip(ns, islice(cycle(build_pattern(base, i + 1)), 1, l + 1)):
            out[i] += n * m
        out[i] = abs(out[i]) % 10
    return out


def fft_repeated(ns):
    new = list(ns)
    curr = sum(ns)
    for i in range(len(ns)):
        new[i] = abs(curr) % 10
        curr -= ns[i]
    return new


def build_pattern(i=1):
    for b in (0, 1, 0, -1):
        for _ in range(i):
            yield b


if __name__ == '__main__':
    with open("16.txt") as f:
        t = f.read().strip()
    ns = tuple(map(int, t))
    offset = int(t[:7])
    # part 1
    # for _ in range(100):
    #     ns = fft(ns)
    # print(ns)
    # print(''.join(str(n) for n in ns[:8]))
    r = 10000
    length = len(ns) * r
    ns = list(islice(cycle(ns), offset, length))
    for _ in range(100):
        ns = fft_repeated(ns)
    print(''.join(str(n) for n in ns[:8]))
