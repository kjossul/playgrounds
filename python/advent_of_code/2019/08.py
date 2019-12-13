from collections import Counter

import numpy as np
from PIL import Image

W, H = 25, 6

if __name__ == '__main__':
    with open("08.txt") as f:
        data = f.read()[:-1]

    N = len(data) // (W * H)
    data = np.array(list(map(int, data)), dtype=np.int8)
    layers = np.split(data, N)
    counts = tuple(Counter(layer) for layer in layers)
    s1 = min(counts, key=lambda c: c[0])
    print(s1[1] * s1[2])
    im = Image.new('1', (W, H))
    for i, ls in enumerate(zip(*layers)):
        y, x = divmod(i, W)
        pixel = tuple(x for x in ls if x != 2)[0]
        im.putpixel((x, y), int(pixel))
    im.show()
