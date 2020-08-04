#! /usr/bin/env python3

from PIL import Image

s = Image.open("16_mozart.gif")
w, h = s.size
t = Image.new(s.mode, (2*w, h))
pink = 195
pxs = list(s.getdata())
for row in range(h):
    ls = pxs[row*w:(row+1)*w]
    start = w - ls.index(pink)
    for x, px in enumerate(ls, start=start):
        t.putpixel((x, row), px)

t.show()  # romance

