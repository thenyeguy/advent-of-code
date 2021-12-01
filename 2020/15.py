data = [6,19,0,5,7,13,1]

def play(seed, num_turns):
    last = None
    last_turn = dict()
    for turn in range(num_turns):
        if turn < len(seed):
            say = seed[turn]
        else:
            say = turn - last_turn.get(last, turn)
        last_turn[last] = turn
        last = say
    return last

print("Part A:", play(data, 2020))
print("Part B:", play(data, 30000000)) 
