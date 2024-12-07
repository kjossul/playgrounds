if __name__ == '__main__':
    with open("06.txt") as f:
        text = f.read().strip()
    i = 0
    l = 14
    while True:
        s = text[i:i+l]
        if len(set(s)) == l:
            break
        i += 1
    print(i + l)