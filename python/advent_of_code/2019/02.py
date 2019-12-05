from itertools import product


def execute(ns, a, b):
    ns = list(ns)
    ns[1], ns[2] = a, b
    for i in range(0, len(ns), 4):
        if ns[i] == 99:
            break
        elif ns[i] == 1:
            ns[ns[i + 3]] = ns[ns[i + 1]] + ns[ns[i + 2]]
        else:
            ns[ns[i + 3]] = ns[ns[i + 1]] * ns[ns[i + 2]]
    return ns[0]


if __name__ == '__main__':
    with open("02.txt") as f:
        ns = list(map(int, f.read().split(',')))
        print(execute(ns, 12, 2))
        for a, b in product(range(100), repeat=2):
            n = execute(ns, a, b)
            if n == 19690720:
                print(100 * a + b)
