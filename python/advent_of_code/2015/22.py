#! /usr/bin/env python3

class Spell:
    def __init__(self, *args, **kwargs):
        self.__dict__.update(**kwargs)
        self.counter = 0

    def cast(self, s, t):
        s.mana -= self.cost
        self.counter = self.duration

    def tick(self, s, t):
        if self.counter > 0:
            self.counter -= 1
            self.apply_effect(s, t)
        else:
            self.wear_off(s, t)

    def apply_effect(self, s, t):
        pass

    def wear_off(self, s, t):
        self.counter = 0

    def __str__(self):
        return repr(self) + str(self.__dict__)

    def __repr__(self):
        return self.__class__.__name__


class MagicMissile(Spell):
    def __init__(self, cost=53, damage=4):
        super().__init__(cost=cost, duration=1)
        self.damage = damage

    def apply_effect(self, s, t):
        t.hit(self.damage)

class Drain(Spell):
    def __init__(self):
        super().__init__(cost=73, duration=1)
        self.damage = 2

    def apply_effect(self, s, t):
        s.hp += 2
        t.hit(self.damage)

class Shield(Spell):
    def __init__(self):
        super().__init__(cost=113, duration=6)
        self.armor = 7

    def cast(self, s, t):
        super().cast(s, t)
        self.saved = s.armor
        s.armor += self.armor

class Poison(Spell):
    def __init__(self):
        super().__init__(cost=173, duration=6)
        self.damage = 3

    def apply_effect(self, s, t):
        t.hit(self.damage)

class Recharge(Spell):
    def __init__(self):
        super().__init__(cost=229, duration=5)
        self.amount = 101

    def apply_effect(self, s, t):
        s.mana += self.amount


class Player:
    def __init__(self, hp, armor, mana, spells, hard=False):
        self.save_hp = self.hp = hp
        self.armor = armor
        self.save_mana = self.mana = mana
        self.spells = spells
        self.hard = hard

    def hit(self, damage):
        self.hp -= max(1, damage - self.armor)

    def get_castable_spells(self):
        return (spell for spell in self.spells if spell.counter == 0 and self.mana >= spell.cost)    
    
    def is_alive(self):
        return self.hp > 0

    def tick(self, enemy):
        for spell in self.spells:
            spell.tick(self, enemy)

    def cast(self, spell, enemy):
        if self.hard:
            self.hp -= 1
        spell.cast(self, enemy)

    def reset(self, enemy):
        self.hp = self.save_hp
        self.mana = self.save_mana
        self.armor = 0
        for spell in self.spells:
            spell.wear_off(self, enemy)

    def __str__(self):
        return str(self.__dict__)


def simulate(hero, boss, spells, verbose=False):
    hero.reset(boss)
    boss.reset(hero)
    a, d = hero, boss
    for spell in spells:
        if verbose:
            print(f"{a}\n{d}\n{spell}")
        a.cast(spell, d)
        a.tick(d)
        d.tick(a)
        if a.hp * d.hp <= 0:
            break
        a, d = d, a
    if verbose:
        print(hero)
    castable = list(hero.get_castable_spells())
    return castable != [] and boss.hp <= 0 and hero.hp > 0, (spells, castable)

def play(hero, boss):
    spells = [simulate(hero, boss, [])]
    best = None
    while spells:
        hero_won, (current, castable) = spells.pop(0)
        if hero_won:
            mana_used = sum(spell.cost for spell in current[::2])
            best = mana_used if best is None else min(best, mana_used)
            return best, current
        else:
            spells.extend(simulate(hero, boss, current + [x, boss.spells[0]]) for x in castable)
    return best


if __name__ == '__main__':
    spells = (MagicMissile(), Drain(), Shield(), Poison(), Recharge())
    boss_atk = MagicMissile(cost=0, damage=9)
    hero = Player(50, 0, 500, spells)
    boss = Player(51, 0, 0, (boss_atk,)) 
    
    # score1, seq = play(hero, boss)
    # print(score1, seq)
    hero.hard = True
    score2, seq = play(hero, boss)
    print(score2, seq)
    simulate(hero, boss, seq, verbose=True)
