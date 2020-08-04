import sys
from math import sqrt

intcode = __import__("09")

if __name__ == '__main__':
    program = intcode.IntCode.from_file("19.txt")
    world = {}
    for i in range(50):
        for j in range(50):
            program.reset()
            program.queue = [i, j]
            program.output_callback = lambda x: world.setdefault((i, j), x)
            program.execute()
    s1 = sum(v for v in world.values())
    print(s1)
    for i in range(50):
        print(''.join('#' if world[(j, i)] == 1 else '.' for j in range(50)))
    row = 5
    start = 3
    end = 5
    program.output_callback = lambda v: setattr(program, "v", v)
    boundary = 100 * sqrt(2)
    candidates = {}
    while True:
        column = start
        while True:
            program.reset()
            program.queue = [column, row]
            program.execute()
            if program.v == 0:
                if column >= end:
                    end = column
                    break
                else:
                    start = column
            column += 1
        # sys.stdout.write(f'\r {row, start, end}')
        if end - start - 1 >= boundary:
            candidates[row] = end
            upper = row - 99
            if upper in candidates:
                x = start + 1
                if x + 99 < candidates[upper]:
                    print(f"\n{x * 10000 + upper}")
                    exit(1)
        row += 1
