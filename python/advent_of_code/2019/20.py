import numpy as np

from solver import Game


class Maze:
    moves = tuple(np.array(t) for t in ((1, 0), (0, -1), (-1, 0), (0, 1)))
    f = lambda s: s.explore()
    t = lambda s: s.is_completed()
    h = lambda s, step: (s.level, step)
    hashfun = lambda s: (s.pos, s.visited_portals)

    def __init__(self, portals, walls, free, pos, goal):
        self.portals = portals
        self.walls = walls
        self.free = free
        self.pos = pos
        self.goal = goal
        self.level = 0
        self.visited_portals = frozenset()
        self.visited = frozenset()

    def copy(self):
        cls = self.__class__
        new = cls.__new__(cls)
        new.__dict__.update(self.__dict__)
        return new

    def explore(self):
        for move in self.moves:
            dest = np.array(self.pos) + move
            t = tuple(dest)
            if (t, self.level) in self.visited:
                continue
            if t in self.portals:
                portal = self.portals[t]
                new = self.copy()
                new.pos = portal.links[t]
                new.level += -1 if portal.outer else 1
                if new.level < 0:
                    continue
                new.visited_portals = self.visited_portals | frozenset([(portal.name, self.level)])
                new.visited = self.visited | frozenset([(self.pos, self.level)])
                yield new
            elif t in self.free:
                new = self.copy()
                new.pos = t
                yield new

    def goal_distance(self):
        return sum(map(abs, np.array(self.pos) - np.array(self.goal)))


    def is_completed(self):
        return self.pos == self.goal and self.level == 0

    @classmethod
    def from_file(cls, filename):
        with open(filename) as f:
            ls = list(map(list, f.read().splitlines()))

        portals = {}
        w, h = len(ls[2]) + 2, len(ls)
        walls = set()
        free = set()
        inner = [0] * 4  # n e s w of inner circle
        for y, line in enumerate(ls):
            for x, c in enumerate(line):
                t = (x, y)
                if c == '#':
                    walls.add(t)
                elif c == '.':
                    free.add(t)
                elif c == ' ' and 2 < y < h - 3 and 2 < x < w - 3:
                    if not any(inner[:2]):
                        inner[0] = y
                        inner[1] = x
                    inner[2] = y
                    inner[3] = x + 1
            if x < w:
                ls[y].extend([' '] * (w - x))
        directions = cls.moves
        d = {
            ((0, 1), (0, h)): directions[0],  # outer left
            ((inner[3] - 2, inner[3] - 1), (inner[0], inner[2])): directions[0],  # inner right
            ((0, w), (h - 1, h - 2)): directions[1],  # outer bottom
            ((inner[1], inner[3]), (inner[0] + 1, inner[0])): directions[1],  # inner top
            ((w - 1, w - 2), (0, h)): directions[2],  # outer right
            ((inner[1] + 1, inner[1]), (inner[0], inner[2])): directions[2],  # inner left
            ((0, w), (0, 1)): directions[3],  # outer top
            ((inner[1], inner[3]), (inner[2] - 1, inner[2])): directions[3],  # inner bottom
        }
        portal_dict = {}
        is_outer = True
        for (boundx, boundy), direction in d.items():
            for portal in cls.parse_portals(ls, *boundx, *boundy, direction, is_outer):
                name = portal.name
                if name in portals:
                    portals[name].link(portal)
                    if name not in 'AAZZ':
                        portal_dict[portal.entrance] = portal
                else:
                    portals[name] = portal
                    if name not in 'AAZZ':
                        portal_dict[portal.entrance] = portal
            is_outer = not is_outer
        free.add(portals['ZZ'].entrance)
        return cls(portal_dict, walls, free, portals['AA'].entrance, portals['ZZ'].entrance)

    @staticmethod
    def parse_portals(lines, startx, endx, starty, endy, direction, is_outer):
        stepx = 1 if startx < endx >= 0 else -1
        stepy = 1 if starty < endy >= 0 else -1
        for y, line in enumerate(lines[starty:endy:stepy], start=starty * stepy):
            for x, c in enumerate(line[startx:endx:stepx], start=startx * stepx):
                if c.isalpha():
                    v = np.array([abs(x), abs(y)])
                    x1, y1 = v + direction
                    c1 = lines[y1][x1]
                    if not c1.isalpha():
                        continue
                    name = ''.join(sorted(c + c1))
                    entrance = tuple(v + direction * 2)
                    yield Portal(name, entrance, is_outer)


class Portal:
    def __init__(self, name, entrance, is_outer):
        self.name = name
        self.entrance = entrance
        self.links = {}
        self.outer = is_outer

    def __str__(self):
        return self.name

    def __repr__(self):
        return str(self)

    def __hash__(self):
        return hash(self.name)

    def link(self, other):
        if other.entrance != self.entrance:
            self.links[self.entrance] = other.entrance
            self.links[other.entrance] = self.entrance
            other.links[other.entrance] = self.entrance
            other.links[self.entrance] = other.entrance


if __name__ == '__main__':
    s0 = Maze.from_file("20.txt")
    game = Game(s0, Maze.f, Maze.t, h=Maze.h, min_h=True, hashfun=Maze.hashfun, verbose=True)
    ss = game.solve()
    print(ss[0].step + len(ss[0].state.visited_portals))
