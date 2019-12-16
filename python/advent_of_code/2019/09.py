from collections import defaultdict


class HaltException(Exception):
    pass


class NoInputException(Exception):
    pass


class IntCode:
    def __init__(self, code, queue=None, input_callback=None, output_callback=print):
        self.ns = code
        self.instructions = defaultdict(int, dict(enumerate(self.ns)))
        self.pc = self.relbase = 0
        self.queue = queue if queue else []
        self.input_callback = input_callback
        self.output_callback = output_callback
        functions = (self.add, self.mul, self.input, self.output, self.jne, self.jeq, self.lt, self.eq, self.rebase)
        self.fs = {f"{op:02}": fun for op, fun in enumerate(functions, start=1)}

    def add(self, *args):
        self.instructions[args[2]] = args[0] + args[1]
        self.pc += 4

    def mul(self, *args):
        self.instructions[args[2]] = args[0] * args[1]
        self.pc += 4

    def input(self, *args):
        try:
            if self.input_callback:
                nextval = self.input_callback()
            else:
                nextval = self.queue.pop(0)
        except IndexError:
            raise NoInputException
        self.instructions[args[0]] = nextval
        self.pc += 2

    def output(self, *args):
        self.output_callback(args[0])
        self.pc += 2

    def jne(self, *args):
        self.pc = args[1] if args[0] != 0 else self.pc + 3

    def jeq(self, *args):
        self.pc = args[1] if args[0] == 0 else self.pc + 3

    def lt(self, *args):
        self.instructions[args[2]] = int(args[0] < args[1])
        self.pc += 4

    def eq(self, *args):
        self.instructions[args[2]] = int(args[0] == args[1])
        self.pc += 4

    def rebase(self, *args):
        self.relbase += args[0]
        self.pc += 2

    def execute(self):
        while True:
            try:
                self.tick()
            except HaltException:
                break

    def tick(self):
        instruction = f"{self.instructions[self.pc]:05}"
        opcode = instruction[3:]
        if opcode == '99':
            raise HaltException
        elif opcode in ('03', '04', '09'):
            is_write = opcode == '03'
            params = (self.get_value(self.instructions[self.pc + 1], instruction[2], is_write),)
        else:
            params = tuple(self.get_value(self.instructions[self.pc + i], mode, i == 3)
                           for i, mode in enumerate(instruction[2::-1], start=1))
        self.fs[opcode](*params)

    def reset(self):
        self.instructions = defaultdict(int, dict(enumerate(self.ns)))
        self.pc = self.relbase = 0

    def copy(self):
        cls = self.__class__
        new = cls(self.ns)
        new.instructions = defaultdict(int, self.instructions)
        new.pc = self.pc
        new.relbase = self.relbase
        return new

    def get_value(self, v, mode, is_write_destination=False):
        if is_write_destination or mode == '1':
            return v if mode != '2' else v + self.relbase
        elif mode == '0':
            return self.instructions[v]
        elif mode == '2':
            return self.instructions[self.relbase + v]

    @classmethod
    def from_file(cls, filename):
        with open(filename) as f:
            data = f.read().strip()
        code = list(map(int, data.split(',')))
        return cls(code)


if __name__ == '__main__':
    program = IntCode.from_file("09.txt")
    program.queue = [2]
    program.execute()
