import math
import pprint
import re
import sys
from collections import defaultdict, Counter
from fractions import Fraction
from statistics import mean

import solver


class FuelFactory:
    def __init__(self, reactions):
        self.reactions = reactions
        self.needed = defaultdict(int)
        self.invert_reactions()

    def __str__(self):
        return str(self.needed)

    def __repr__(self):
        return str(self)

    def produce(self, fuel=1):
        # memoization
        self.needed.clear()
        self.needed['FUEL'] = fuel
        while any(v > 0 for k, v in self.needed.items() if k != 'ORE'):
            for k, v in self.needed.copy().items():
                if v <= 0 or k == 'ORE':
                    continue
                for reaction in self.reactions:
                    if k in reaction.reagents:
                        repeat = math.ceil(v / reaction.reagents[k])
                        self.apply_reaction(reaction, repeat)
        return self.needed['ORE']

    def find_fuel(self, ore):
        low, hi = 0, 1e9
        while low < hi:
            mid = mean([low, hi])
            o = self.produce(mid)
            if o > ore:
                hi = mid - 1
            else:
                low = mid
        return round(low)

    def apply_reaction(self, reaction, times, d=None):
        d = d if d else self.needed
        for k, v in reaction.reagents.items():
            d[k] -= v * times
        for k, v in reaction.products.items():
            d[k] += v * times

    def invert_reactions(self):
        for reaction in self.reactions:
            reaction.invert()

    @classmethod
    def from_file(cls, filename):
        p = re.compile(r"(\d+) (\w+)")
        reactions = []
        with open(filename) as f:
            for line in f.readlines():
                reactions.append(Reaction(*map(p.findall, line.split(" => "))))
        return cls(reactions)


class Reaction:
    def __init__(self, l, r):
        self.reagents = {name: int(q) for q, name in l}
        self.products = {name: int(q) for q, name in r}

    def __str__(self):
        return str(self.__dict__)

    def __repr__(self):
        return str(self)

    def invert(self):
        self.reagents, self.products = self.products, self.reagents

    def is_appliable(self, items, times=1):
        return all(abs(items[k]) >= v * times for k, v in self.reagents.items())


if __name__ == '__main__':
    factory = FuelFactory.from_file("14.txt")
    print(factory.produce())
    print(factory.find_fuel(1e12))
