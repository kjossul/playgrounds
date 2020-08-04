#! /usr/bin/env python3

import requests

def get_next(v):
    url = "http://www.pythonchallenge.com/pc/def/linkedlist.php?nothing=" + v
    r = requests.get(url)
    ws = r.content.split()
    print(ws)
    return str(ws[-1].decode("utf-8"))


# x = get_next("12345")
x = get_next("8022")  # prints peak.html
while x:
    x = get_next(x)
