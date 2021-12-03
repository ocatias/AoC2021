with open("input.txt") as f:
    lines = f.readlines()
    get_int = lambda text: int(text.split(" ")[1])
    states = list(map(lambda l:
        (get_int(l) if "forward" in l else 0,
        -get_int(l) if "up" in l else
            get_int(l) if "down" in l else 0) , lines))

    x_changes, y_changes = list(zip(*states))
    print(sum(x_changes)*sum(y_changes))

    state = [0,0,0]
    for step in lines:
        X = get_int(step)
        if "down" in step:
            state[2] += X
        elif "up" in step:
            state[2] -= X
        else:
            state[0] += X
            state[1] += state[2]*X
    print(state[0]*state[1])
