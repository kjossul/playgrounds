import re

if __name__ == '__main__':
    pairs = []
    with open("04.txt") as f:
        for line in f:
            ns = list(map(int, re.findall(r'\d+', line)))
            s1 = set(range(ns[0], ns[1] + 1))
            s2 = set(range(ns[2], ns[3] + 1))
            pairs.append((s1, s2))

    print(sum(s1.issubset(s2) or s1.issuperset(s2) for s1, s2 in pairs))
    print(sum(len(s1.intersection(s2)) > 0 for s1, s2 in pairs))
