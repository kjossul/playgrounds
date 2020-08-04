#! /usr/bin/env python3

import datetime

def is_leap(y):
    if y % 4 is not 0:
        return False
    elif y % 100 is not 0:
        return True
    elif y % 400 is not 0:
        return False
    else:
        return True

leaps = [y for y in range(1000, 2000) if is_leap(y) and y % 10 == 6 and datetime.date(y, 1, 26).weekday() is 0]
print(leaps[-2])  # 1756/01/26 -> Birth of Mozart
