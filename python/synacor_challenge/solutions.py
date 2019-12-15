import operator
from collections import defaultdict
from copy import deepcopy
from functools import wraps
from itertools import permutations

import numpy as np

from challenge import Arch, HaltException, Register


class VaultState:
    STARTPOS = (3, 0)
    ENDPOS = (0, 3)
    MOVES = np.array([(0, 1), (-1, 0), (0, -1), (1, 0)])
    DESCRIPTIONS = ("east\n", "north\n", "west\n", "south\n")
    ROOMS = np.matrix([[operator.mul, 8, operator.sub, 1],
                       [4, operator.mul, 11, operator.mul],
                       [operator.add, 4, operator.sub, 18],
                       [22, operator.sub, 9, operator.mul]])
    TARGET = 30

    def __init__(self, orb, moves, pos, history=None):
        self.orb = orb
        self.moves = moves
        self.pos = pos
        self.history = history + [self.orb] if history else [self.orb]

    @classmethod
    def init(cls):
        orb = cls.ROOMS[cls.STARTPOS]
        moves = ""
        pos = cls.STARTPOS
        return cls(orb, moves, pos)

    @classmethod
    def solve(cls):
        visited = set()
        stack = [cls.init()]
        while True:
            state = stack.pop(0)
            for next_state in state.get_next_states():
                if next_state.is_completed():
                    with open("vault.txt", 'w') as f:
                        f.write(next_state.moves)
                        f.write("vault\ntake mirror\nuse mirror")
                        # Don't forget to reverse the code you get ;)
                    print(next_state.history)
                    return
                elif next_state not in visited and tuple(next_state.pos) != cls.ENDPOS:
                    visited.add(next_state)
                    stack.append(next_state)

    def get_next_states(self):
        for m1, d1 in zip(self.MOVES, self.DESCRIPTIONS):
            t = self.pos + m1
            if np.all((t >= 0) & (t < 4)):
                operation = self.ROOMS[tuple(t)]
                for m2, d2 in zip(self.MOVES, self.DESCRIPTIONS):
                    new_pos = t + m2
                    if np.all((new_pos >= 0) & (new_pos < 4)) and tuple(new_pos) != self.STARTPOS:
                        operand = self.ROOMS[tuple(new_pos)]
                        new_orb = operation(self.orb, operand)
                        new_state = VaultState(new_orb, self.moves + d1 + d2, new_pos, self.history)
                        yield new_state

    def is_completed(self):
        return np.all(self.pos == self.ENDPOS) and self.orb == self.TARGET

    def __hash__(self):
        return hash((self.orb, tuple(self.pos)))


def memoize(f):
    f.d = {}

    @wraps(f)
    def _f(*args, **kwargs):
        try:
            return f.d[args]
        except KeyError:
            f.d[args] = f(*args, **kwargs)
            return f.d[args]

    return _f


def ruins():
    coins = (2, 3, 5, 7, 9)
    for p in permutations(coins):
        a, b, c, d, e = p
        result = a + b * c ** 2 + d ** 3 - e
        if result == 399:
            return p


@memoize
def ackerman(n, a, b):
    """
    if A is 0:
        return B + 1
    else:
        if B is 0:
            return f(A-1, N)
        else:
            return f(A-1, f(A, B-1))
    """
    if a == 0:
        return b + 1
    elif a == 1:
        return n + b + 1
    elif a == 2:
        return b * (n + 1) + 2 * n + 1
    elif a == 3:
        t = n
        for _ in range(b + 1):
            t = t * (n + 1) + 2 * n + 1
            t %= Arch.UPPER
        return t
    elif a == 4 and b == 1:  # only call
        return ackerman(n, 3, ackerman(n, 3, n))


def find_teleporter():
    for n in range(1, Arch.UPPER):
        v = ackerman(n, 4, 1) % Arch.UPPER
        if v == 6:
            print(f"\nFound: {n}")
            break
        print(f"Checking: {n:5}", end='\r')


if __name__ == '__main__':
    VaultState.solve()
