from collections import defaultdict
from itertools import chain
from statistics import mean

import numpy as np

from solver import Game

intcode = __import__("09")


class State:
    WORLD = None
    INTERSECTIONS = []
    MOVES = dict(zip("^v<>", list(np.array(t) for t in ((-1, 0), (1, 0), (0, -1), (0, 1)))))
    VISITABLE = set()
    f = lambda s: s.explore()
    t = lambda s: s.is_completed()
    h = lambda s, step: len(s.visited)
    hashfun = lambda s: tuple(s.directions)

    def __init__(self, pos, facing, directions, visited=None, intersections=None, main_program=None):
        self.pos = pos
        self.facing = facing
        self.directions = directions
        self.visited = visited if visited else set()
        self.intersections = intersections if intersections else defaultdict(int)
        self.main_program = main_program if main_program else Program()

    def __str__(self):
        return ','.join(str(i) for i in self.directions)

    def __repr__(self):
        return str(self)

    def explore(self):
        t = tuple(self.pos)
        if t in self.INTERSECTIONS:
            self.intersections[t] += 1
            if self.is_invalid():
                raise StopIteration
            self.visited.add(t)

            self.pos += self.MOVES[self.facing]
            self.directions[-1] += 1
            self.complete_branch()
            yield self


        else:
            self.complete_branch()
            yield self

    def complete_branch(self):
        has_moved = True
        while tuple(self.pos) not in self.INTERSECTIONS and has_moved:
            has_moved = False
            self.visited.add(tuple(self.pos))
            for facing, move in self.MOVES.items():
                dest = self.pos + move
                # avoids coming back
                if np.all(dest >= 0) and np.all(dest < self.WORLD.shape) and self.WORLD[tuple(dest)] == '#' \
                        and tuple(move + self.MOVES[self.facing]) != (0, 0):
                    self.pos = dest
                    has_moved = True
                    if self.facing == facing:
                        self.directions[-1] += 1
                    else:
                        if self.facing + facing in "^<v>^":
                            turn = 'L'
                        else:
                            turn = 'R'
                        self.main_program.add_instruction(tuple(self.directions[-2:]))
                        self.directions.append(turn)
                        self.facing = facing
                        self.directions.append(1)
                    break

    def find_rotation(self, i):
        s = "^<v>" * 2
        j = s.index(self.facing)
        return s[j + i]

    def is_completed(self):
        return self.visited == self.VISITABLE and \
               self.main_program.build(list(zip(self.directions[::2], self.directions[1::2])), str(self))

    def is_invalid(self):
        return any(x > 4 for x in self.intersections.values()) or not self.main_program.valid or len(str(self)) > 200 \
               or sum(x < 6 for x in self.directions[-7:-1:2]) > 0


class Program:
    def __init__(self):
        self.followers = defaultdict(set)
        self.current = None
        self.valid = True
        self.i = 0
        self.programs = []
        self.main_function = None

    def copy(self):
        new = Program()
        new.__dict__.update(self.__dict__)
        return new

    def add_instruction(self, instruction):
        if self.i > 15:
            self.valid = self.valid and instruction in self.followers[self.current]
            self.current = instruction
        else:
            if self.current:
                self.followers[self.current].add(instruction)
            self.current = instruction
            self.i += 1
            return True

    def build(self, instructions, code):
        programs = []
        indexes = {}
        start = 0
        limit = 4
        for c in "ABC":
            if c == 'C':  # 3rd program is forced
                limit = min(start - i - 1 for i in indexes if start - i > 0)
            results = self.longest_subprogram(instructions, start=start, limit=limit)
            program = instructions[results[0][0]:results[0][1]]
            programs.append(program)
            for lo, hi in results:
                indexes[lo] = c
                if start == lo:
                    start = hi
        d = {}
        for name, program in zip("ABC", programs):
            subroutine_code = ','.join(f"{turn},{amount}" for turn, amount in program) + '\n'
            d[name] = subroutine_code[:-1]
            self.programs.append(tuple(map(ord, subroutine_code)))
        main_function = [value for key, value in sorted(indexes.items())]
        main_code = ','.join(main_function) + '\n'
        self.main_function = tuple(map(ord, main_code))
        built_code = ','.join(d[c] for c in main_function)
        return code == built_code

    def longest_subprogram(self, instructions, start=0, limit=4):
        longest = 0
        j = start
        results = []
        while True:
            try:
                j = instructions.index(instructions[start], j + 1)
            except (ValueError, IndexError):
                break
            for k, v2 in enumerate(instructions[j:j + limit] + [None], start=start):
                if instructions[k] != v2 or k >= j:
                    results.append((j, k + j - start))
                    break
            longest = max(k - start, longest)
        out = [(start, start + longest)] if j != start else [(start, min(limit, len(instructions)))]
        for x, y in results:
            if y - x == longest and x >= out[-1][1]:  # avoids overlapping
                out.append((x, y))
        return out


if __name__ == '__main__':
    program = intcode.IntCode.from_file("17.txt")
    ascii_codes = []
    cb = lambda x: ascii_codes.append(x)
    program.output_callback = cb
    program.execute()
    s = ''.join(chr(c) for c in ascii_codes).strip()
    print(s)
    State.WORLD = np.stack(list(map(list, s.splitlines())))
    w, h = State.WORLD.shape

    pos = None
    direction = None
    for i, line in enumerate(State.WORLD):
        for j, c in enumerate(line):
            v = np.array([i, j])
            if c in State.MOVES:
                pos = v
                direction = c
                State.VISITABLE.add(tuple(v))
            if c == '#':
                State.VISITABLE.add(tuple(v))
                if 0 < i < w - 1 and 0 < j < h - 1 \
                        and all(State.WORLD[tuple(v + move)] == '#' for move in State.MOVES.values()):
                    State.INTERSECTIONS.append(tuple(v))

    alignment = sum(np.prod(i) for i in State.INTERSECTIONS)
    print(alignment, pos)
    program.ns[0] = 2
    map_length = len(ascii_codes)
    ascii_codes.clear()


    def print_live_feed(v):
        if v > 1000:
            print(v)
            return
        ascii_codes.append(v)
        if len(ascii_codes) <= map_length:
            pass
        else:
            s = ''.join(chr(c) for c in ascii_codes)
            ascii_codes.clear()
            print(s)


    program.output_callback = print_live_feed
    program.reset()
    s0 = State(pos, direction, [])
    game = Game(s0, State.f, State.t, h=State.h, hashfun=State.hashfun, solution_limit=1)
    solution = game.solve()
    movement_program = solution[0][0][-1].main_program
    program.queue.extend(movement_program.main_function)
    program.queue.extend(chain(*movement_program.programs))
    program.queue.extend([ord('n'), ord('\n')])
    program.execute()
