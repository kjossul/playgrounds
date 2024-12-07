if __name__ == '__main__':
    with open("01.txt") as f:
        elves = f.read().split('\n\n')
        calories = [sum(int(food) for food in elf.splitlines()) for elf in elves]
        calories = sorted(calories, reverse=True)
        print(calories[0])
        print(sum(calories[:3]))
