import re

p1 = re.compile(r'(\d)\1')
p2 = re.compile(r'(\d)\1{2,}')


def check(n):
    s = str(n)
    s1 = p2.sub('', s)
    c1 = p1.search(s1) is not None
    c2 = s == ''.join(sorted(s))
    return c1 and c2


if __name__ == '__main__':
    s1 = sum(check(n) for n in range(254032, 789861))
    print(s1)
