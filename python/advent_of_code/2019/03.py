def add_coords(x1, y1, x2, y2):
    return x1 + x2, y1 + y2


def manhattan(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)


def get_coords(wire):
    d = {'R': (1, 0), 'U': (0, 1), 'L': (-1, 0), 'D': (0, -1)}
    curr = (0, 0)
    coords = set()
    i = 1
    steps = {}
    for ins in wire.split(','):
        direction = d[ins[0]]
        for _ in range(int(ins[1:])):
            curr = add_coords(*curr, *direction)
            coords.add(curr)
            steps[curr] = i
            i += 1
    return coords, steps


if __name__ == '__main__':
    with open("03.txt") as f:
        wires = f.readlines()
        coords, steps = zip(*(get_coords(wire) for wire in wires))
        crosspoints = set.intersection(*coords)
        s1 = min(manhattan(0, 0, *c) for c in crosspoints)
        s2 = min(sum(step[c] for step in steps) for c in crosspoints)
        print(s1, s2)

