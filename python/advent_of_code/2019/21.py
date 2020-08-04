intcode = __import__("09")


class SpringScript:
    def __init__(self):
        self.t = self.j = False


if __name__ == '__main__':
    program = intcode.IntCode.from_file("21.txt")
    out = []
    # Part 1 Logic: D & ~(A & B & C)
    program.output_callback = lambda v: out.append(v)
    instructions = ["OR A T", "AND B T", "AND C T", "NOT T T", "OR D J", "AND T J"]
    part1 = '\n'.join(instructions) + "\nWALK\n"
    program.queue = list(map(ord, part1))
    program.execute()
    print(''.join(map(chr, out[:-1])))
    print(out[-1])
    # part 2 Logic: D & ~(A & B & C) & ~(~E & ~H) == D & .. & (E | H)
    program.reset()
    out.clear()
    instructions = ["OR A T", "AND B T", "AND C T", "NOT T T", "OR E J", "OR H J", "AND J T", "NOT D J",
                    "NOT J J", "AND T J"]
    part2 = '\n'.join(instructions) + "\nRUN\n"
    program.queue = list(map(ord, part2))
    program.execute()
    print(''.join(map(chr, out[:-1])))
    print(out[-1])
