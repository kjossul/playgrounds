#! /usr/bin/env python3

from PIL import Image

im = Image.open("11_cave.jpg")
w, h = im.size
o = im.copy()
e = im.copy()
for x in range(w):
    for y in range(h):
        t = e if (x+y) % 2 == 0 else o
        t.putpixel((x,y), (0,0,0,0))

o.show()  # evil
