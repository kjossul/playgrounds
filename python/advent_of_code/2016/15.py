class Disc:
    def __init__(self, n, start):
        self.n = n
        self.pos = start

    def is_aligned(self, t):
        """Whether this disc will be aligned at time t"""
        return (self.pos + t) % self.n is 0


if __name__ == '__main__':
    DISCS1 = tuple(Disc(n, start) for n, start in ((17, 1), (7, 0), (19, 2), (5, 0), (3, 0), (13, 5)))
    DISCS2 = DISCS1 + (Disc(11, 0),)
    t = 0
    while not all(d.is_aligned(t+delta) for delta, d in enumerate(DISCS2, start=1)):
        t += 1
    print(t)
