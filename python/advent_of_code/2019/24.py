import numpy as np

from search import GridNode, TreeSearch, Node, Directions


class Conway(GridNode):
    def __init__(self, level_manager=None, **kwargs):
        super().__init__(**kwargs)
        self.level_manager = level_manager
        self.centre = (self.width // 2, self.height // 2)
        self.bugs = set()

    def copy(self):
        cls = self.__class__
        new = cls.__new__(cls)
        new.__dict__.update(self.__dict__)
        new.bugs = set(self.bugs)
        return new

    def explore(self):
        self.next_gen()
        yield self

    def next_gen(self, level=None):
        new_bugs = set(self.bugs)
        new_bio = 0
        for i, t in enumerate(self.yield_grid()):
            bugs = sum(neighbour in self.bugs for neighbour in self.yield_neighbours(centre=t))
            if self.level_manager:
                if t == self.centre:
                    continue
                else:
                    bugs += self.level_manager.count_outside_bugs(level=level, xy=t)
            if t in self.bugs:
                if bugs != 1:
                    new_bugs.remove(t)
                else:
                    new_bio += 2 ** i
            else:
                if 1 <= bugs <= 2:
                    new_bugs.add(t)
                    new_bio += 2 ** i
        self.bugs = new_bugs
        # print(new_bio)

    def is_goal(self):
        return False

    @property
    def key(self):
        return frozenset(self.bugs)

    @classmethod
    def from_file(cls, filename, **kwargs):
        return super().from_file(filename, mapping={"#": "bugs"}, remove_whitespace=True)


class RecursiveConway(Node):
    def __init__(self, s0):
        super().__init__()
        self.levels = {0: s0}
        s0.level_manager = self
        self.width = s0.width
        self.height = s0.height
        self.centre = s0.centre
        self.epochs = 0

    def explore(self):
        super().explore()
        new_levels = {}
        low = min(self.levels)
        high = max(self.levels)
        if self.levels[low].bugs:
            self.levels[low - 1] = self.empty_conway()
        if self.levels[high].bugs:
            self.levels[high + 1] = self.empty_conway()
        for level, conway in self.levels.items():
            copy = conway.copy()
            copy.next_gen(level=level)
            new_levels[level] = copy
        self.levels.update(new_levels)
        yield self

    def is_goal(self):
        return self._depth == self.epochs

    @property
    def key(self):
        return frozenset((level, conway.key) for level, conway in self.levels.items())

    def total_bugs(self):
        return sum(len(level.bugs) for level in self.levels.values())

    def count_outside_bugs(self, level, xy):
        x, y = xy
        bugs = 0
        outer = self.levels.get(level - 1, self.empty_conway())
        inner = self.levels.get(level + 1, self.empty_conway())
        points = Directions._make(tuple(np.array(self.centre) + move) for move in GridNode.MOVES)
        # outer
        if x == 0:
            bugs += points.W in outer.bugs
        if x == self.width - 1:
            bugs += points.E in outer.bugs
        if y == 0:
            bugs += points.N in outer.bugs
        if y == self.height - 1:
            bugs += points.S in outer.bugs
        # inner
        if xy == points.E:
            bugs += sum(1 for bug in inner.bugs if bug[0] == self.width - 1)
        if xy == points.W:
            bugs += sum(1 for bug in inner.bugs if bug[0] == 0)
        if xy == points.N:
            bugs += sum(1 for bug in inner.bugs if bug[1] == 0)
        if xy == points.S:
            bugs += sum(1 for bug in inner.bugs if bug[1] == self.height - 1)
        return bugs

    def empty_conway(self):
        return Conway(level_manager=self, width=self.width, height=self.height)

    @classmethod
    def from_file(cls, filename):
        s0 = Conway.from_file(filename)
        return cls(s0=s0)


if __name__ == '__main__':
    s0 = Conway.from_file("24.txt")
    search = TreeSearch(s0)
    search.solve()
    # part2
    manager = RecursiveConway.from_file("24.txt")
    manager.epochs = 200
    s2 = TreeSearch(manager, memoize=False)
    solution = s2.solve()[0]
    print(solution.total_bugs())
