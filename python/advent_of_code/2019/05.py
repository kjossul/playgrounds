INPUT = 1


def add(ns, i, *args):
    ns[args[2]] = args[0] + args[1]
    return i + 4


def mul(ns, i, *args):
    ns[args[2]] = args[0] * args[1]
    return i + 4


def start(ns, i, *args):
    ns[args[0]] = INPUT
    return i + 2


def output(ns, i, *args):
    print(args[0])
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


def execute(ns):
    i = 0
    functions = (add, mul, start, output, jne, jeq, lt, eq)
    fs = {f"{op:02}": fun for op, fun in enumerate(functions, start=1)}
    while True:
        instruction = f"{ns[i]:05}"
        opcode = instruction[3:]
        if opcode == '99':
            break
        elif opcode == '03':
            params = (ns[i + 1],)
        elif opcode == '04':
            params = (ns[ns[i + 1]],) if instruction[2] == '0' else (ns[i+1],)
        else:
            params = (ns[i + j] if mode == '1' or j == 3 else ns[ns[i + j]]
                      for j, mode in enumerate(instruction[2::-1], start=1))
        i = fs[opcode](ns, i, *params)
    return ns[0]


if __name__ == '__main__':
    with open("05.txt") as f:
        ns = list(map(int, f.read().split(',')))
        execute(ns)
