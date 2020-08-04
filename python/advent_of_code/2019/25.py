import pickle
import sys
from itertools import combinations

intcode = __import__("09")


def i_callback(program):
    if not program.queue:
        x = input("> ") + '\n'
        if x == "save\n":
            with open("25.sav", "wb") as f:
                program.input_callback = None
                program.output_callback = None
                pickle.dump(program, f)
                program.input_callback = lambda: i_callback(program)
                program.output_callback = lambda v: sys.stdout.write(chr(v))
            return i_callback(program)
        elif x == "load\n":
            with open("25.sav", "rb") as f:
                other = pickle.load(f)
            program.instructions = other.instructions
            program.pc = other.pc
            program.relbase = other.relbase
            return i_callback(program)
        elif x == 'solve\n':
            items = ["space heater", "semiconductor", "planetoid", "hypercube", "spool of cat6", "sand",
                     "festive hat", "dark matter"]
            queue = []
            for i in range(4, 9):
                for c in combinations(items, r=i):
                    for item in c:
                        queue.extend(f"take {item}\n")
                    queue.extend("west\n")
                    for item in c:
                        queue.extend(f"drop {item}\n")
            program.queue.extend(map(ord, queue))
        else:
            program.queue.extend(map(ord, x))
    return program.queue.pop(0)


if __name__ == '__main__':
    program = intcode.IntCode.from_file("25.txt")
    program.input_callback = lambda: i_callback(program)
    program.output_callback = lambda v: sys.stdout.write(chr(v))
    program.execute()
