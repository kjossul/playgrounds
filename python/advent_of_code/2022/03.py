from dataclasses import dataclass


def priority(c):
    if 'A' <= c <= 'Z':
        return ord(c) - 38
    else:
        return ord(c) - 96


@dataclass
class Rucksack:
    line: str

    def item_priority(self):
        l = len(self.line) // 2
        a = self.line[:l]
        b = self.line[l:]
        item = (set(a) & set(b)).pop()
        return priority(item)


if __name__ == '__main__':
    with open("03.txt") as f:
        rucksacks = [Rucksack(line) for line in f.read().splitlines()]
    print(sum(r.item_priority() for r in rucksacks))
    s = 0
    for i in range(0, len(rucksacks), 3):
        item = set.intersection(*(set(r.line) for r in rucksacks[i:i+3])).pop()
        s += priority(item)
    print(s)
