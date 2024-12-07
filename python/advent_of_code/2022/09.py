import numpy as np


def step(h, t):
    x_diff = h[0] - t[0]
    y_diff = h[1] - t[1]
    if x_diff == 0:
        if abs(y_diff) > 1:
            t[1] += np.sign(y_diff)
    elif y_diff == 0:
        if abs(x_diff) > 1:
            t[0] += np.sign(x_diff)
    elif abs(x_diff) + abs(y_diff) > 2:
        t[0] += np.sign(x_diff)
        t[1] += np.sign(y_diff)


if __name__ == '__main__':
    visited = {(0, 0)}
    h = np.array([0, 0])
    t = np.array([0, 0])
    instructions = {
        'R': np.array([1, 0]),
        'U': np.array([0, 1]),
        'L': np.array([-1, 0]),
        'D': np.array([0, -1])
    }

    with open("09.txt") as f:
        for line in f.readlines():
            i, n = line.split()
            for _ in range(int(n)):
                h += instructions[i]
                step(h, t)
                visited.add(tuple(t))
    print(len(visited))
    visited = {(0, 0)}
    knots = [np.array([0, 0]) for _ in range(10)]
    with open("09.txt") as f:
        for line in f.readlines():
            i, n = line.split()
            for _ in range(int(n)):
                knots[0] += instructions[i]
                for j in range(9):
                    step(knots[j], knots[j+1])
                visited.add(tuple(knots[-1]))
    print(len(visited))
