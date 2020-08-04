#! /usr/bin/env python3

from PIL import Image
from itertools import cycle

out = Image.new('RGB', (100, 100))
wire = Image.open("14_wire.png")

fs = [
        lambda x,y: (x+1, y),
        lambda x,y: (x, y-1),
        lambda x,y: (x-1, y),
        lambda x,y: (x, y+1),
        ]
visited = set()
x = y = 0
fs_i = cycle(fs)
f = next(fs_i)
for pixel in wire.getdata():
    out.putpixel((x, y), pixel)
    visited.add((x, y))
    xn, yn = f(x, y)
    if not 0 <= xn < 100 or not 0 <= yn < 100 or (xn, yn) in visited:
        f = next(fs_i)
        x, y = f(x, y)
    else:
        x, y = xn, yn

out.show()

