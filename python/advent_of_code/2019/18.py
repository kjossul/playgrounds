from collections import defaultdict
from math import ceil

import numpy as np

from solver import Game


class JointSearch:
    f = lambda s: s.explore()
    t = lambda s: s.is_completed()
    h = lambda s, step: (len(s.ordered_keys), -s.steps)
    hashfun = lambda s: s.ordered_keys
    memo = defaultdict(set)

    def __init__(self, robots=None, steps=0, ordered_keys=None):
        self.robots = robots if robots else []
        for robot in robots:
            robot.controller = self
        self.steps = steps
        self.ordered_keys = ordered_keys if ordered_keys else tuple()

    def explore(self):
        if self.steps > 1800:
            raise StopIteration
        for i, robot in enumerate(self.robots):
            r = robot.copy()
            old_keys = r.owned
            t = lambda x: len(x.owned) > len(old_keys)
            key = State.hashfun(r)
            if key in self.memo:
                for solution in self.memo[key]:
                    found = solution.state.owned - old_keys
                    new_robots = [r.copy() for r in self.robots]
                    new_robots[i] = solution.state.copy()
                    yield JointSearch(new_robots, steps=self.steps + solution.step,
                                      ordered_keys=self.ordered_keys + (found.pop(),))
                continue
            game = Game(r, State.f, t, hashfun=State.hashfun, solution_limit=None, verbose=False)
            s = game.solve()
            for solution in s:
                self.memo[key].add(solution)
                found = solution.state.owned - old_keys
                new_robots = [r.copy() for r in self.robots]
                new_robots[i] = solution.state.copy()
                yield JointSearch(new_robots, steps=self.steps + solution.step,
                                  ordered_keys=self.ordered_keys + (found.pop(),))

    def is_completed(self):
        return len(self.robots[0].keys) - sum(len(r.owned) for r in self.robots) == 0

    def is_reachable(self, robot, pos):
        return pos not in robot.walls and (pos not in robot.doors or
                                           any(robot.doors[pos] in r.owned for r in self.robots))

    @classmethod
    def from_file(cls, filename):
        robots = []
        with open(filename) as f:
            ls = f.readlines()
            n = ceil(len(ls) / 2) - 1
            m = ceil(len(ls[0]) / 2) - 1
            robots.extend([State.read_text(ls) for _ in range(4)])
            for robot, start in zip(robots, [(n - 1, m - 1), (n + 1, m - 1), (n - 1, m + 1), (n + 1, m + 1)]):
                robot.start = start
                robot.pos = start
        return JointSearch(robots)


class State:
    f = lambda s: s.explore()
    t = lambda s: s.is_completed()
    h = lambda s, step: len(s.owned)
    hashfun = lambda s: (s.pos, frozenset(s.owned))

    def __init__(self, start, keys, doors, walls, bounds, pos=None, owned=None, controller=None):
        self.start = start
        self.keys = keys
        self.doors = doors
        self.walls = walls
        self.bounds = bounds
        self.pos = pos if pos else start
        self.owned = owned if owned else set()
        self.controller = controller

    def __str__(self):
        return str(State.hashfun(self))

    def __repr__(self):
        return str(self)

    def copy(self):
        new = State(self.start, self.keys, self.doors, self.walls, self.bounds, owned=self.owned,
                    controller=self.controller, pos=self.pos)
        return new

    def explore(self):
        pos = np.array(self.pos)
        for move in (np.array(x) for x in ((0, 1), (-1, 0), (0, -1), (1, 0))):
            dest = pos + move
            t = tuple(dest)
            is_reachable = self.is_reachable(t) if not self.controller else self.controller.is_reachable(self, t)
            if np.all(dest > 0) and np.all(dest < self.bounds) and is_reachable:
                new = self.copy()
                if t in self.keys and self.keys[t] not in self.owned:
                    new.owned = self.owned | {self.keys[t]}
                new.pos = t
                yield new

    def is_completed(self):
        return len(self.owned) == len(self.keys)

    def is_reachable(self, pos):
        return pos not in self.walls and (pos not in self.doors or self.doors[pos] in self.owned)

    @classmethod
    def from_file(cls, filename, controller=None):
        with open(filename) as f:
            instance = cls.read_text(f.readlines(), controller=controller)
        return instance

    @classmethod
    def read_text(cls, text, controller=None):
        start = None
        keys = {}
        doors = {}
        walls = set()
        for i, line in enumerate(text):
            for j, c in enumerate(line.strip()):
                t = (i, j)
                if c == '@':
                    start = t
                elif c == '#':
                    walls.add(t)
                elif c.islower():
                    keys[t] = c
                elif c.isupper():
                    doors[t] = c.lower()
        return cls(start, keys, doors, walls, bounds=t, pos=start, controller=controller)


if __name__ == '__main__':
    # s0 = State.from_file("18.txt")
    # game = Game(s0, State.f, State.t, hashfun=State.hashfun, solution_limit=1)
    # s = game.solve()[0]
    # print(s.step)
    s0 = JointSearch.from_file("18_bis.txt")
    game = Game(s0, JointSearch.f, JointSearch.t, h=JointSearch.h, hashfun=JointSearch.hashfun, verbose=True)
    ss = game.solve()
    print(min(s.state.steps for s in ss))
