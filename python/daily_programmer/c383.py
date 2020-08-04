from collections import deque, defaultdict, Counter
from itertools import combinations


def same_necklace(s1, s2):
    if set(s1) == set(s2) and len(s1) == len(s2):
        q = deque(s2)
        for _ in range(len(s1)):
            if s1 == ''.join(q):
                return True
            q.rotate(1)
    return False


def repeats(s):
    q = deque(s)
    t = 0
    for _ in range(len(s)):
        t += ''.join(q) == s
        q.rotate(1)
    return t


def check_list():
    words = defaultdict(list)
    with open("enable1.txt") as f:
        for word in f.read().splitlines():
            words[tuple(sorted(Counter(word).items()))].append(word)

    for k, v in words.items():
        if len(v) > 1:
            pairs = combinations(v, r=2)
            result = set()
            for pair in pairs:
                if same_necklace(*pair):
                    result.update(pair)
            if len(result) == 4:
                print(result)
                return


if __name__ == '__main__':
    assert same_necklace("nicole", "icolen")
    assert not same_necklace("abc", "cba")
    assert repeats("abcabcabc") == 3
    check_list()
