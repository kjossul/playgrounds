#! /usr/bin/env python3

import re
from itertools import combinations

PLAYER_STATS = ('hp', 'damage', 'armor')
ITEM_STATS = ('cost', 'damage', 'armor')


class Player:
    def __init__(self, *args, **kwargs):
        d = {k: int(v) for k,v in zip(PLAYER_STATS, args)}
        self.__dict__.update(d)
        self.__dict__.update(**kwargs)
        self.save = self.hp

    def hit(self, enemy):
        enemy.hp -= max(1, self.damage - enemy.armor)

    def reset(self):
        self.hp = self.save

    @classmethod
    def from_items(cls, *items, hp=100):
        d = {x[0]: sum(x[1:]) for x in zip(ITEM_STATS, *items)}
        return cls(hp=hp, **d)

    @staticmethod
    def play(p1, p2):
        attack, defend = p1, p2
        while p1.hp * p2.hp > 0:
            attack.hit(defend)
            attack, defend = defend, attack
        p2.reset()  # resets enemy
        return p1.hp > 0

    def __str__(self):
        return str(self.__dict__)

def solve(enemy, items):
    boss, store = parse(enemy, (i.splitlines()[1:] for i in items.split('\n\n')))
    m = M = None
    for c in yield_items(*store):
        player = Player.from_items(*c)
        if Player.play(player, boss):
            m = player.cost if m is None else min(m, player.cost)
        else:
            M = player.cost if M is None else max(M, player.cost)
    return m, M

def yield_items(weapons, armors, rings):
    for w in weapons:
        for i in range(3):
            for r in combinations(rings, i):
                yield list(r) + [w]  # armor is not mandatory
                for a in armors:
                    yield list(r) + [w] + [a]

def parse(enemy, items):
    p = re.compile("\d+")
    boss = Player(*p.findall(enemy))
    p = re.compile(" (\d+)\s+(\d+)\s+(\d+)")
    store = [[list(map(int, p.findall(x)[0])) for x in xs] for xs in items]
    return boss, store

if __name__ == '__main__':
    with open("21_items.txt") as f:
        items = f.read()
    with open("21_stats.txt") as f:
        enemy = f.read()
    
    s1 = solve(enemy, items)
    print(s1)


