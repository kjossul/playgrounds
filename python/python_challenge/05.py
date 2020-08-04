#! /usr/bin/env python3

import pickle

with open("05_banner.p", 'rb') as f:
    xs = pickle.load(f)
    t = ''
    for x in xs:
        for c, n in x:
            t += c*n
        t += '\n'
    print(t)
