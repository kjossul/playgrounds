#! /usr/bin/env python3

from PIL import Image
from io import BytesIO

with open("12_evil2.gfx", 'rb') as f:
    bs = f.read()

ds = [bs[i::5] for i in range(5)]
for i, d in enumerate(ds):
    stream = BytesIO(d)
    im = Image.open(stream)
    try:
        im.save(f"12_out{i}.png")
    except OSError:
        pass

