from collections import defaultdict

import numpy as np

from solver import Game

intcode = __import__("09")


class Robot:
    MOVEMENT = tuple(np.array(x) for x in ((0, 1), (0, -1), (1, 0), (-1, 0)))
    s0 = (0, 0)

    def __init__(self, program):
        self.programs = {(0, 0): program}
        self.world = defaultdict(int)

    def fill_oxygen(self):
        i = 0
        while 0 in self.world.values():
            new_world = self.world.copy()
            for k, v in new_world.items():
                pos = np.array(k)
                if v != -1 and any(self.world[tuple(pos + move)] == 1 for move in self.MOVEMENT):
                    new_world[k] = 1
            self.world = new_world
            i += 1
        return i

    def output_callback(self, dest, v):
        self.world[tuple(dest)] = v - 1

    def explore(self, pos):
        for i, move in enumerate(self.MOVEMENT, start=1):
            dest = np.array(pos) + move
            t = tuple(dest)
            program = self.programs[pos]
            new_program = program.copy()
            new_program.queue.append(i)
            new_program.output_callback = lambda v: self.output_callback(dest, v)
            self.programs[t] = new_program
            try:
                new_program.execute()
            except intcode.NoInputException:
                pass
            if self.world[t] != -1:
                yield t

    def is_goal(self, pos):
        return self.world[tuple(pos)] == 1


if __name__ == '__main__':
    program = intcode.IntCode.from_file("15.txt")
    robot = Robot(program)

    game = Game(robot.s0, robot.explore, robot.is_goal, stop=False)
    s = game.solve()
    print(s)
    time = robot.fill_oxygen()
    print(time)
