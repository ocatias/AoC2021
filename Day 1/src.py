with open('input.txt') as f:
    numbers = [int(x) for x in f.read().split("\n")[:-1]]
    prev, increases = numbers[0], 0
    for number in numbers[1:]:
        if number > prev:
            increases += 1
        prev = number
    print("Increases: {0}".format(increases))

    sums = [sum(numbers[i:i+3]) for i in range(0, len(numbers) -2)]
    prev, increases = sums[0], 0

    for sum in sums[1:]:
        if sum > prev:
            increases += 1
        prev = sum
        
    print("Increases: {0}".format(increases))
