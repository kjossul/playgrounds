#! /usr/bin/env python3

import requests
import bz2
import xmlrpc.client
from urllib.parse import unquote_to_bytes

info = []
url = "http://www.pythonchallenge.com/pc/def/linkedlist.php?busynothing="
def get_next(v):
    r = requests.get(url+v)
    ws = r.content.split()
    print(ws, r.cookies)
    for cookie in r.cookies:
        info.append(cookie.value)
    return str(ws[-1].decode("utf-8"))

"""
x = get_next("12345")
while x.isnumeric():
    x = get_next(x)

info = info.replace('+', ' ')
info = unquote_to_bytes(''.join(info))
print(bz2.decompress(info))
# b'is it the 26th already? call his father and inform him that "the flowers are on their way". he\'ll understand.'
"""

url = "http://www.pythonchallenge.com/pc/phonebook.php"
with xmlrpc.client.ServerProxy(url) as proxy:
    print(proxy.phone("Leopold"))  # Mozart's father

url = "http://www.pythonchallenge.com/pc/stuff/violin.php"
cookies = {'info': 'the flowers are on their way'}
r = requests.post(url, cookies=cookies)
print(r.content)
