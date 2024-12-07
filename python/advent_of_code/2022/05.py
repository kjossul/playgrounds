import re

if __name__ == '__main__':
    with open("05.txt") as f:
        setup, instructions = f.read().split('\n\n')
    stacks = [[] for _ in range(9)]
    for line in setup.splitlines()[:-1]:
        for i in range(9):
            j = 1 + i * 4
            try:
                c = line[j]
            except IndexError:
                break
            if c != ' ':
                stacks[i].insert(0, c)
    for instruction in instructions.splitlines():
        n, a, b = map(int, re.findall(r'\d+', instruction))
        """ part1
        for _ in range(n):
            e = stacks[a - 1].pop()
            stacks[b - 1].append(e)
        """
        es = stacks[a-1][-n:]
        stacks[a-1] = stacks[a-1][:-n]
        stacks[b-1].extend(es)
    print(''.join(stack[-1] for stack in stacks))

