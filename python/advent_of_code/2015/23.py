#! /usr/bin/env python3

class Register:
    def __init__(self, value):
        self.value = value

    def hlf(self):
        self.value = self.value // 2
        return 1

    def tpl(self):
        self.value *= 3
        return 1

    def inc(self):
        self.value += 1
        return 1

    def jmp(self, offset):
        return offset

    def jie(self, offset):
        return offset if self.value % 2 == 0 else 1

    def jio(self, offset):
        return offset if self.value == 1 else 1


if __name__ == '__main__':
    registers = {
        'a': Register(1),
        'b': Register(0)
    }
    with open("23.txt") as f:
        pc = 0
        ls = f.read().splitlines()
        ins = []
        for l in ls:
            ws = l.split()
            if ws[0] == 'jmp':
                ins.append((registers['a'].jmp, [int(ws[1])]))
            elif len(ws) == 3:
                ins.append((getattr(registers[ws[1][:-1]], ws[0]), [int(ws[-1])]))
            else:
                ins.append((getattr(registers[ws[1]], ws[0]), []))
        while pc < len(ins):
            f, args = ins[pc]
            result = f(*args)
            pc += result
    print(registers['b'].value)
