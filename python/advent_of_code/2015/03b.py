#! /usr/bin/env python3

def solve(t):
    houses = {(0, 0)}
    s = [[0, 0], [0, 0]]
    parity = 0
    # (element to modify, value to add)
    a = {
            'v': (1, -1),
            '^': (1, 1),
            '<': (0, -1),
            '>': (0, 1)
        }
    for c in t:
        s[parity][a[c][0]] += a[c][1]
        houses.add(tuple(s[parity]))
        parity = not parity
    return len(houses)

if __name__ == '__main__':
    assert solve("^v") == 3
    assert solve("^>v<") == 3
    assert solve("^v^v^v^v^v") == 11

    with open("03.txt") as f:
        print(solve(f.read()))

