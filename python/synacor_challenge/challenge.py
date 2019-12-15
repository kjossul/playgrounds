import inspect
import pickle
import datetime
from collections import defaultdict, Counter


class HaltException(Exception):
    pass


def opcode(funclist):
    def decorator(f):
        funclist.append(f)
        return f

    return decorator


class Arch:
    UPPER = 32768
    FUNCTIONS = []
    LOGFILE = None

    def __init__(self, code, verbose=False):
        self.descriptions = {}
        self.code = code
        self.pc = 0
        self.registers = [Register() for _ in range(8)]
        self.stack = []
        self.buffer = []
        self.visited = set()
        self.arities = tuple(len(inspect.signature(f).parameters) - 1 for f in self.FUNCTIONS)
        self.verbose = verbose
        self.LOGFILE = open("history.log", 'w')
        self.LOGFILE.truncate(0)
        self.LOGFILE.seek(0)


    @classmethod
    def from_file(cls, filename, savefile=None, verbose=False):
        with open(filename, 'rb') as f:
            instructions = []
            data = f.read(2)
            while data:
                instructions.append(int.from_bytes(data, byteorder='little', signed=False))
                data = f.read(2)
        program = Arch(instructions, verbose=verbose)
        if savefile:
            program.load(savefile)
        return program

    def input_callback(self):
        return input('> ') + '\n'

    def output_callback(self, a):
        print(chr(a), end='')

    def save(self, filename):
        with open(filename, 'wb') as f:
            pickle.dump(self, f)
        print(f"Successfully saved to {filename}.")

    def load(self, filename):
        try:
            with open(filename, 'rb') as f:
                state = pickle.load(f)
            self.__dict__.update(state.__dict__)
            print(f"Successfully loaded save file {filename}.")
        except FileNotFoundError:
            print(f"File {filename} does not exist.")

    def read(self, filename):
        try:
            with open(filename) as f:
                self.buffer = list(f.read())
            print(f"Successfully read file {filename}")
        except FileNotFoundError:
            print(f"File {filename} does not exist.")

    def debg(self, filename):
        with open(filename, 'w') as f:
            raise NotImplementedError

    def get_value(self, v):
        if v < self.UPPER:
            return Register(v)  # creates new dummy register (literal value)
        else:
            i = v - self.UPPER
            if i < 0:
                raise ValueError
            return self.registers[i]

    def execute(self):
        while True:
            try:
                if self.pc == 5480:
                    # 5498 is the correct destination of the Beach (teleportation successful)
                    self.registers[-1].value = 25734  # magic number for register[7]
                    self.pc = 5498
                opcode = self.code[self.pc]
                args = tuple(self.get_value(self.code[self.pc + i]) for i in range(1, self.arities[opcode] + 1))
                result = self.FUNCTIONS[opcode](self, *args)
                if result:
                    self.pc = result
                else:
                    self.pc += self.arities[opcode] + 1
            except HaltException:
                break

    def dump_instructions(self, filename="dump.txt"):
        with open(filename, 'w') as f:
            pc = 0
            while pc < self.UPPER:
                try:
                    op = self.code[pc]
                    arity = self.arities[op]
                    description = f"{self.FUNCTIONS[op].__name__:6s} "
                    description += ' '.join(str(arg) if arg < self.UPPER else f"R[{arg - self.UPPER}]"
                                            for arg in self.code[pc + 1:pc + 1 + arity])
                    f.write(f"{pc:5}: {description}\n")
                    self.descriptions[pc] = description
                    pc += arity + 1
                except IndexError:
                    pc += 1

    @opcode(FUNCTIONS)
    def halt(self):
        raise HaltException

    @opcode(FUNCTIONS)
    def set(self, a, b):
        a.value = b.value

    @opcode(FUNCTIONS)
    def push(self, a):
        self.stack.append(a.value)

    @opcode(FUNCTIONS)
    def pop(self, a):
        a.value = self.stack.pop()

    @opcode(FUNCTIONS)
    def eq(self, a, b, c):
        a.value = int(b.value == c.value)

    @opcode(FUNCTIONS)
    def gt(self, a, b, c):
        a.value = int(b.value > c.value)

    @opcode(FUNCTIONS)
    def jmp(self, a):
        return a.value

    @opcode(FUNCTIONS)
    def jt(self, a, b):
        if a.value != 0:
            return b.value

    @opcode(FUNCTIONS)
    def jf(self, a, b):
        if a.value == 0:
            return b.value

    @opcode(FUNCTIONS)
    def add(self, a, b, c):
        a.value = (b.value + c.value) % self.UPPER

    @opcode(FUNCTIONS)
    def mul(self, a, b, c):
        a.value = (b.value * c.value) % self.UPPER

    @opcode(FUNCTIONS)
    def mod(self, a, b, c):
        a.value = b.value % c.value

    @opcode(FUNCTIONS)
    def andb(self, a, b, c):
        a.value = b.value & c.value

    @opcode(FUNCTIONS)
    def orb(self, a, b, c):
        a.value = b.value | c.value

    @opcode(FUNCTIONS)
    def notb(self, a, b):
        """15-bit bitwise not"""
        a.value = (1 << 15) - 1 - b.value

    @opcode(FUNCTIONS)
    def rmem(self, a, b):
        a.value = self.code[b.value]

    @opcode(FUNCTIONS)
    def wmem(self, a, b):
        self.code[a.value] = b.value

    @opcode(FUNCTIONS)
    def call(self, a):
        self.stack.append(self.pc + 2)
        return a.value

    @opcode(FUNCTIONS)
    def ret(self):
        try:
            return self.stack.pop()
        except ValueError:
            raise HaltException

    @opcode(FUNCTIONS)
    def output(self, a):
        self.output_callback(a.value)

    @opcode(FUNCTIONS)
    def input(self, a):
        if not self.buffer:
            self.buffer.extend(self.input_callback())
        operation = ''.join(self.buffer[:4])
        if hasattr(self, operation):
            filename = ''.join(self.buffer[5:-1])
            self.buffer.clear()
            getattr(self, operation)(filename)
            self.input(a)
        else:
            a.value = ord(self.buffer.pop(0))

    @opcode(FUNCTIONS)
    def noop(self):
        pass


class Register:
    def __init__(self, value=0):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return str(self)


if __name__ == '__main__':
    program = Arch.from_file("challenge.bin", savefile="vault.sav", verbose=True)
    program.registers[-1].value = 40
    program.dump_instructions()
    program.execute()
