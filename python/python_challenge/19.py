#! /usr/bin/env python3

import requests
import re
import base64
from email import message_from_bytes

url = "http://butter:fly@www.pythonchallenge.com/pc/hex/bin.html"
r = requests.get(url)
p = re.compile(b'<!--\n(.*)\n-->', flags=re.DOTALL | re.MULTILINE)
m = p.search(r.content)
msg = message_from_bytes(m.group(1))
attach = msg.get_payload(0)
audio = attach.get_payload(decode=True)

with open("19_indian.wav", 'wb') as f:
    f.write(audio)


