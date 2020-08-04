import heapq
from abc import ABC, abstractmethod
from collections import namedtuple

import numpy as np

Directions = namedtuple("Directions", "E N W S")


class TreeSearch:
    def __init__(self, root, solutions=1, memoize=True):
        self.queue = PriorityMemQueue(memoize=memoize)
        self.queue.push(root)
        self.solutions = solutions

    def solve(self):
        solutions = []
        while True:
            try:
                node = self.queue.pop()
            except IndexError:
                print(f"No more states to observe")
                return solutions
            if node.is_goal():
                solutions.append(node)
                if len(solutions) >= self.solutions:
                    return solutions
            else:
                for node in node.explore():
                    self.queue.push(node)


class Node(ABC):
    def __init__(self, depth=0):
        self._depth = depth

    @abstractmethod
    def explore(self):
        self._depth += 1

    @abstractmethod
    def is_goal(self):
        pass

    @property
    @abstractmethod
    def key(self):
        pass

    @property
    def heuristic(self):
        """Default heuristic: BFS"""
        return self._depth


class GridNode(Node, ABC):
    MOVES = tuple(np.array(x) for x in Directions((1, 0), (0, -1), (-1, 0), (0, 1)))

    def __init__(self, position=None, width=None, height=None, depth=0):
        super().__init__(depth=depth)
        self.position = position
        self.width = width
        self.height = height

    def yield_grid(self):
        for y in range(self.height):
            for x in range(self.width):
                yield (x, y)

    def yield_neighbours(self, centre=None):
        v = np.array(centre if centre else self.position)
        for move in self.MOVES:
            dest = v + move
            if np.all(dest >= 0) and np.all(dest < (self.width, self.height)):
                yield tuple(dest)

    @classmethod
    def from_file(cls, filename, mapping=None, remove_whitespace=False):
        with open(filename) as f:
            ls = f.readlines()
        d = {k: set() for k in (mapping.values() if mapping else ())}
        for y, line in enumerate(ls):
            if remove_whitespace:
                line = line.strip()
            for x, c in enumerate(line):
                try:
                    k = mapping[c]
                    d[k].add((x, y))
                except KeyError:
                    pass
        instance = cls(position=None, width=x+1, height=y+1)
        instance.__dict__.update(d)
        return instance


class PriorityMemQueue:
    def __init__(self, highest=False, memoize=True):
        self._queue = []
        self.mul = -1 if highest else 1
        self.visited = set()

    def push(self, node: Node):
        priority = node.heuristic * self.mul
        if node.key not in self.visited:
            heapq.heappush(self._queue, (priority, node))
            self.visited.add(node.key)

    def pop(self):
        return heapq.heappop(self._queue)[-1]
