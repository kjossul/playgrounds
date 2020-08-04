#! /usr/bin/env python3

def bisection(f, a, b):
    x = (a + b) / 2
    if f(x) == 0:
        return x
    elif f(x)*f(a) < 0:
        return bisection(f, a, x)
    else:
        return bisection(f, x, b)

if __name__ == '__main__':
    print(bisection(lambda x: x**2 - x, 0.5, 2))

