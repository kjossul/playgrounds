#! /usr/bin/env python3

from PIL import Image

y = 48
step = 7

im = Image.open("07_oxygen.png")
w, h = im.size
vs = []
for x in range(1, w-24, step):
    v = im.getpixel((x, y))
    vs.append(v[0])

print(''.join(chr(v) for v in vs))
s = [105, 110, 116, 101, 103, 114, 105, 116, 121]
print(''.join(chr(v) for v in s))
