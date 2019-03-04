#!/usr/bin/env python3

def insertion_sort(xs):
    for j, x in enumerate(xs[1:], start=1):
        i = j - 1
        while i >= 0 and xs[i] > x:
            xs[i + 1] = xs[i] 
            i -= 1
        xs[i + 1] = x
    return xs

def merge_sort(xs):
    """ Recursively merge sub-lists"""
    def merge(xs, ys):
        result = []
        while xs and ys:
            if xs[0] < ys[0]:
                result.append(xs.pop(0))
            else:
                result.append(ys.pop(0))
        #  Finish adding one of the two lists that is not yet empty
        while xs:
            result.append(xs.pop(0))
        while ys:
            result.append(ys.pop(0))
        return result

    l = len(xs)
    if l < 2:  # at most one element: we have finished
        return xs
    else:
        split = l // 2
        return merge(merge_sort(xs[:split]), merge_sort(xs[split:]))

def test(fs):
    TEST_CASES = {
        () : (), # empty list
        (0,): (0,), # single element
        (0, 1, 2, 3, 4, 5, 6, 7, 8, 9): (0, 1, 2, 3, 4, 5, 6, 7, 8, 9),  # reversed order
        (7, 1, 7, 3, 4, 1, 6, 7, 8, 8): (1, 1, 3, 4, 6, 7, 7, 7, 8, 8),  # repeated elements
    }

    for f in fs:
        for actual, expected in TEST_CASES.items():
            result = f(list(actual))
            try:
                assert result == list(expected)
            except AssertionError:
                print(f"{result} != {list(expected)}")
                raise AssertionError
    print("Success!")

if __name__ == '__main__':
    fs = (insertion_sort, merge_sort)
    test(fs)
