import numpy as np

iterations = 200

def neighbors(x,y):
    n = [(x + a, b + y) for a in [+1,0,-1] for b in [+1,0,-1]]
    n.remove((x,y))
    return list(filter(lambda xy: xy[0] >= 0 and xy[0] <= 9 and xy[1] >= 0 and xy[1] <= 9, n))

def update(board, has_exploded, x, y):
    board[x,y] += 1
    if board[x,y] > 9 and has_exploded[x,y] == 0:
        has_exploded[x,y] = 1

        for (x2,y2) in neighbors(x,y):
            if has_exploded[x2,y2] == 0:
                update(board, has_exploded, x2,y2)

coordinates = [(x % 10, int(x / 10)) for x in range(100)]

with open('input.txt') as f:
    input = list(map(lambda string: [int(s) for s in string] , f.read().split("\n")[:-1]))

    board = np.array(input)
    has_exploded = np.zeros_like(board)
    explosions, iter = 0, 0
    found_min_syn = False

    while iter < 100 or not found_min_syn:
        iter += 1
        has_exploded = np.zeros_like(board)
        for (x,y) in coordinates:
                update(board, has_exploded, x, y)

        for (x,y) in coordinates:
            if board[x,y] > 9:
                board[x,y] = 0

        new_explosions = np.sum(has_exploded)
        explosions += np.sum(has_exploded)

        if new_explosions == 100 and not found_min_syn:
            print("Synchronized explosion:", iter)
            found_min_syn = True

        if iter == 100:
            print("Explosions: ", explosions)
