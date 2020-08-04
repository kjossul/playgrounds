#! /usr/bin/env python3

from zipfile import ZipFile

with ZipFile("06_channel.zip", 'r') as z:
    x = "90052"
    comments = ''
    while x.isnumeric():
        with z.open(f"{x}.txt") as f:
            comments += z.getinfo(f"{x}.txt").comment.decode()
            t = f.read()
            x = t.split()[-1].decode()
    print(comments)

