from itertools import combinations

import numpy as np


def energy(position, velocity):
    return sum(abs(x) for x in position) * sum(abs(x) for x in velocity)


if __name__ == '__main__':
    startpos = ((14, 2, 8), (7, 4, 10), (1, 17, 16), (-4, -1, 1))
    positions = list(np.array(x) for x in startpos)
    velocities = list(np.array([0, 0, 0]) for _ in range(len(positions)))
    step = 0
    periods = [None] * 3
    while not all(periods):
        # apply gravity
        for a, b in combinations(range(4), r=2):
            for axis in range(3):
                if positions[a][axis] < positions[b][axis]:
                    velocities[a][axis] += 1
                    velocities[b][axis] -= 1
                elif positions[a][axis] > positions[b][axis]:
                    velocities[b][axis] += 1
                    velocities[a][axis] -= 1
        # apply velocity
        for moon in range(4):
            positions[moon] += velocities[moon]
        step += 1
        for axis in range(3):
            if all(s[axis] == s0[axis] and v[axis] == 0 for s, v, s0 in zip(positions, velocities, startpos)):
                print(f"Axis {axis} restored at time {step}")
                if not periods[axis]:
                    periods[axis] = step
    print(np.lcm.reduce(periods))
    # print(sum(energy(*x) for x in zip(positions, velocities)))
