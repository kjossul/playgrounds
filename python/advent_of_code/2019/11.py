from collections import defaultdict
from importlib import import_module
import numpy as np

intcode = import_module("09")
world = defaultdict(int)
position = np.array([0, 0])
DIRECTIONS = tuple(np.array(c) for c in ((0, 1), (-1, 0), (0, -1), (1, 0)))
direction = 0
is_paint_instruction = True


def input_callback():
    return world[tuple(position)]


def output_callback(v):
    global direction, position, is_paint_instruction
    if is_paint_instruction:
        world[tuple(position)] = v
    else:
        v = -1 if v is 0 else 1
        direction = (direction + v) % 4
        position += DIRECTIONS[direction]
    is_paint_instruction = not is_paint_instruction


if __name__ == '__main__':
    with open("11.txt") as f:
        data = f.read().strip()
    program = intcode.IntCode(data, input_callback=input_callback, output_callback=output_callback)
    world[(0, 0)] = 1  # part 2
    program.execute()
    print(len(world))
    Ny = 8
    Nx = 50
    for y in range(Ny, -Ny, -1):
        print(''.join('#' if world[(x, y)] else ' ' for x in range(Nx, -Nx, -1)))
