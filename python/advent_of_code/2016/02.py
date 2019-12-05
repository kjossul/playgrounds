#! /usr/bin/env python3

pad2 = {(0,0): '5', 
        (1, -1): '2', (1,0): '6', (1, 1): 'A',
        (2, -2): '1', (2, -1): '3', (2, 0): '7', (2, 1): 'B', (2, 2): 'D',
        (3, -1): '4', (3, 0): '8', (3, 1): 'C',
        (4, 0): '9'}

def move(start, instructions, pad=[(x,y) for x in range(3) for y in range(3)]):
    moves = {'R': (1, 0), 'U': (0, -1), 'L': (-1, 0), 'D': (0, 1)}
    for c in instructions:
        new = (start[0]+moves[c][0], start[1]+moves[c][1])
        if new in pad:
            start = new
    return start

def coord1(c):
    return c[1]*3 + c[0]%3 + 1

def coord2(c):
    return pad2[c]

with open("02.txt") as f:
    start1 = (1, 1)
    start2 = (0, 0)
    ns1 = []
    ns2 = []
    for instructions in f.readlines():
        start1 = move(start1, instructions[:-1])
        start2 = move(start2, instructions[:-1], pad2)
        ns1.append(str(coord1(start1)))
        ns2.append(str(coord2(start2)))
    print(''.join(ns1))
    print(''.join(ns2))

