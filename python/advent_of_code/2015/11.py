#! /usr/bin/env python3

import string

def solve(s):
    while not check(s):
        s = increment(s)
    return s
    
def increment(s):
    if not s:
        return ''
    elif s[-1] != 'z':
        return s[:-1] + chr(ord(s[-1])+1)
    else:
        return increment(s[:-1]) + 'a'

def check(s):
    lc = string.ascii_lowercase
    triads = {lc[i:i+3] for i in range(len(lc)-2)}
    return (all(c not in s for c in "iol") and 
            any(t in s for t in triads) and
            sum(s.count(c*2) >= 1 for c in lc) >= 2)

if __name__ == '__main__':
    assert check("hijklmmn") is False
    assert check("abcdffaa") is True
    assert solve("abcdefgh") == "abcdffaa"
    print("Tests passed")

    x = solve("cqjxjnds")
    print(x, solve(increment(x)))

