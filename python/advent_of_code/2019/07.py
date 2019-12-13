from itertools import permutations

queue = []


class HaltException(Exception):
    pass


def add(ns, i, *args):
    ns[args[2]] = args[0] + args[1]
    return i + 4


def mul(ns, i, *args):
    ns[args[2]] = args[0] * args[1]
    return i + 4


def start(ns, i, *args):
    ns[args[0]] = queue.pop(0)
    return i + 2


def output(ns, i, *args):
    queue.append(args[0])
    return i + 2


def jne(ns, i, *args):
    return args[1] if args[0] != 0 else i + 3


def jeq(ns, i, *args):
    return args[1] if args[0] == 0 else i + 3


def lt(ns, i, *args):
    ns[args[2]] = int(args[0] < args[1])
    return i + 4


def eq(ns, i, *args):
    ns[args[2]] = int(args[0] == args[1])
    return i + 4


def execute(ns, pointer=0):
    functions = (add, mul, start, output, jne, jeq, lt, eq)
    fs = {f"{op:02}": fun for op, fun in enumerate(functions, start=1)}
    while pointer < len(ns):
        instruction = f"{ns[pointer]:05}"
        opcode = instruction[3:]
        if opcode == '99':
            raise HaltException
        elif opcode == '03':
            params = (ns[pointer + 1],)
        elif opcode == '04':
            params = (ns[ns[pointer + 1]],) if instruction[2] == '0' else (ns[pointer + 1],)
        else:
            params = (ns[pointer + j] if mode == '1' or j == 3 else ns[ns[pointer + j]]
                      for j, mode in enumerate(instruction[2::-1], start=1))
        pointer = fs[opcode](ns, pointer, *params)
        if opcode == '04':
            return pointer


if __name__ == '__main__':
    with open("07.txt") as f:
        ns = list(map(int, f.read().split(',')))
    max_signal = None
    for i, c in enumerate(permutations(range(5, 10))):
        codes = tuple(list(ns) for _ in range(5))
        settings = list(c)
        queue.clear()
        pointers = [0] * 5
        queue.append(0)  # first amp input
        terminated = 0
        while True:
            for amp in range(5):
                try:
                    queue.insert(0, settings.pop(0))
                except IndexError:
                    pass
                try:
                    pointers[amp] = execute(codes[amp], pointer=pointers[amp])
                except HaltException:
                    terminated += 1
            if terminated == 5:
                break
        out = queue.pop(0)
        max_signal = max(max_signal, out) if max_signal is not None else out
    print(max_signal)
