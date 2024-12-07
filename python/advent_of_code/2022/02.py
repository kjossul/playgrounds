from dataclasses import dataclass


@dataclass
class Round:
    opponent: int
    response: int

    def outcome(self):
        if self.opponent == self.response:
            return 3
        if (self.opponent == 1 and self.response == 2) or (self.opponent == 2 and self.response == 3) or (
                self.opponent == 3 and self.response == 1):
            return 6
        else:
            return 0

    def part_1_score(self):
        return self.outcome() + self.response

    def part_2_score(self):
        # response is now expected result
        outcome = (self.response - 1) * 3
        if self.response == 1:
            return outcome + (self.opponent - 1) if self.opponent != 1 else 3
        elif self.response == 2:
            return outcome + self.opponent
        else:
            return outcome + 1 + self.opponent % 3


if __name__ == '__main__':
    conversion = {'A': 1, 'X': 1, 'B': 2, 'Y': 2, 'C': 3, 'Z': 3}
    with open("02.txt") as f:
        rounds = [Round(conversion[line[0]], conversion[line[2]]) for line in f.readlines()]

    print(sum(r.part_1_score() for r in rounds))
    print(sum(r.part_2_score() for r in rounds))
