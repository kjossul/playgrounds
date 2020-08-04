#! /usr/bin/env python3

def look_and_say(s):
    x, n = s[0], 1
    out = ''
    for c in s[1:]:
        if c == x:
            n += 1
        else:
            out += str(n) + x
            n = 1
            x = c
    out += str(n) + x
    return out

x = "1"
for i in range(30):
    x = look_and_say(x)

print(len(x))  # 5808
