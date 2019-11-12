#! /usr/bin/env python3

def solve(n):
    x = "1113122113"
    for i in range(n):
        x = look_and_say(x)
    return x

def look_and_say(s):
    i = 0
    out = ""
    c = s[0]
    l = 0  # length of current sequence
    while i < len(s):
        if s[i] == c:
            l += 1
        else:
            out += str(l) + c
            c = s[i]
            l = 1
        i += 1
    out += str(l) + c
    return out

if __name__ == '__main__':
    assert look_and_say("1") == "11"
    assert look_and_say("11") == "21"
    assert look_and_say("21") == "1211"
    assert look_and_say("1211") == "111221"

    s1 = solve(40)
    s2 = solve(50)
    print(len(s1), len(s2))
