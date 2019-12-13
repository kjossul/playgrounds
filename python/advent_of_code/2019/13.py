from collections import defaultdict

intcode = __import__("09")


class Solver:
    def __init__(self):
        self.world = defaultdict(int)
        self.outputs = []
        self.scores = []

    def output_callback(self, out):
        self.outputs.append(out)
        if len(self.outputs) is 3:
            x, y, t = self.outputs
            if x == -1 and y == 0:
                self.scores.append(t)
            else:
                self.world[(x, y)] = t
            self.outputs.clear()

    def input_callback(self):
        """Keeps the paddle below the ball"""
        xp, _ = [c for c, v in self.world.items() if v == 3][0]
        xb, _ = [c for c, v in self.world.items() if v == 4][0]
        if xp > xb:
            return -1
        elif xp == xb:
            return 0
        else:
            return 1


if __name__ == '__main__':
    with open("13.txt") as f:
        data = f.read().strip()
    s = Solver()
    program = intcode.IntCode(data, input_callback=s.input_callback, output_callback=s.output_callback)
    program.execute()
    print(len([x for x in s.world.values() if x == 2]))
    program.reset()
    program.instructions[0] = 2
    program.execute()
    print(s.scores[-1])
