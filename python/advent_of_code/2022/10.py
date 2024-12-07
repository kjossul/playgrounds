class CPU:
    def __init__(self):
        self.cycle = 1
        self.x = 1
        self.strength = 0
        self.buffer = []

    def add_cycle(self):
        pos = self.cycle % 40
        if abs(pos - 1 - self.x) <= 1:
            self.buffer.append('#')
        else:
            self.buffer.append('.')
        if pos == 0:
            print(''.join(self.buffer))
            self.buffer.clear()
        if (self.cycle - 20) % 40 == 0:
            self.strength += self.cycle * self.x
        self.cycle += 1

    def noop(self):
        self.add_cycle()

    def addx(self, n):
        self.add_cycle()
        self.add_cycle()
        self.x += n


if __name__ == '__main__':
    cpu = CPU()

    with open("10.txt") as f:
        for line in f.readlines():
            instruction = line[:4]
            if instruction == 'noop':
                cpu.noop()
            elif instruction == 'addx':
                n = int(line[5:])
                cpu.addx(n)

    print(cpu.strength)
