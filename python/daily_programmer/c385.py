# https://www.reddit.com/r/dailyprogrammer/comments/hrujc5/20200715_challenge_385_intermediate_the_almost/

import numpy as np


class Board:
    SIZE = 64  # guaranteed to be solved 
    BITS = np.math.ceil(np.log2(SIZE))

    def __init__(self):
        self.coins = np.random.randint(2, size=self.SIZE)
        self.key = np.random.randint(self.SIZE)

    def get_parity_array(self):
        return np.array([self.parity(i) for i in range(self.BITS - 1, -1, -1)])

    def parity(self, n):
        return sum(x for i, x in enumerate(self.coins) if (1 << n) & i) % 2

    @staticmethod
    def to_int(arr):
        return arr.dot(2 ** np.arange(arr.size)[::-1])

    def to_bin(self, n):
        return np.fromstring(np.binary_repr(n, width=self.BITS), dtype='S1').astype(int)

    def flip(self, i):
        self.coins[i] = not self.coins[i]

    def prisoner1(self, i):
        x = self.get_parity_array()
        y = self.to_bin(i)
        z = np.logical_xor(x, y)
        return self.to_int(z)

    def prisoner2(self):
        return self.to_int(self.get_parity_array())

    def solve(self):
        coin = self.prisoner1(self.key)
        self.flip(coin)
        assert self.prisoner2() == self.key
        print(f"Guessed number {self.key} by flipping coin #{coin}")


if __name__ == '__main__':
    b = Board()
    b.solve()
