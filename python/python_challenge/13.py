#! /usr/bin/env python3

import xmlrpc.client

url = "http://www.pythonchallenge.com/pc/phonebook.php"

with xmlrpc.client.ServerProxy(url) as proxy:
    print(proxy.phone("Bert"))

