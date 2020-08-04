import re
import sys


class SpaceDeck:
    def __init__(self, n, card=None):
        self.n = n
        self.card = card
        self.offset = 0
        self.increment = 1

    def parse_instructions(self, ls):
        p = re.compile("[a-z\s]+")
        for line in ls:
            name = '_'.join(p.match(line).group(0).split())
            method = getattr(self, name)
            arg = p.sub('', line)
            if arg:
                method(int(arg))
            else:
                method()

    def deal_with_increment(self, m):
        self.increment *= pow(m, self.n - 2, self.n)
        self.increment %= self.n

    def deal_into_new_stack(self):
        self.increment *= -1
        self.increment %= self.n
        self.offset += self.increment
        self.offset %= self.n

    def cut(self, n):
        self.offset += self.increment * n
        self.offset %= self.n

    def get_value(self, n):
        return (self.offset + (self.increment * n)) % self.n

    def get_card_position(self):
        for i in range(self.n):
            t = self.get_value(i)
            if t == self.card:
                return i
        else:  # no break
            raise ValueError("Not Found")

    def repeat(self, n):
        new_increment = pow(self.increment, n, self.n)
        self.offset = self.offset * (1 - new_increment) * pow((1 - self.increment) % self.n, self.n - 2, self.n)
        self.increment = new_increment
        self.offset %= self.n


if __name__ == '__main__':
    with open("22.txt") as f:
        ls = f.readlines()
    n = 10007
    m = 2019
    deck = SpaceDeck(n, m)
    deck.parse_instructions(ls)
    print(deck.get_card_position())
    n = 119315717514047
    m = 2020
    repeat = 101741582076661
    deck = SpaceDeck(n)
    deck.parse_instructions(ls)
    deck.repeat(repeat)
    print(deck.get_value(m))
