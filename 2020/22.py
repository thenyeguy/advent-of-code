with open("data/22.txt", "r") as f:
    def read_deck():
        deck = []
        f.readline()
        line = f.readline().strip()
        while line:
            deck.append(int(line))
            line = f.readline().strip()
        return deck

    player1 = read_deck()
    player2 = read_deck()


# Part A:
def score(deck):
    return sum(card * (len(deck) - i) for i, card in enumerate(deck))

def play_combat(player1, player2):
    player1 = list(player1)
    player2 = list(player2)
    while player1 and player2:
        card1 = player1.pop(0)
        card2 = player2.pop(0)
        if card1 > card2:
            player1.append(card1)
            player1.append(card2)
        else:
            player2.append(card2)
            player2.append(card1)
    return score(player1 or player2)

print("Part A:", play_combat(player1, player2))


# Part B:
def play_recursive_combat(player1, player2):
    def _play(player1, player2):
        player1 = list(player1)
        player2 = list(player2)
        seen_states = set()
        while player1 and player2:
            state = hash(str(player1) + str(player2))
            if state in seen_states:
                return (1, player1)
            else:
                seen_states.add(state)

            card1 = player1.pop(0)
            card2 = player2.pop(0)
            if len(player1) < card1 or len(player2) < card2:
                winner = 1 if card1 > card2 else 2
            else:
                winner, _ = _play(player1[:card1], player2[:card2])

            if winner == 1:
                player1.append(card1)
                player1.append(card2)
            else:
                player2.append(card2)
                player2.append(card1)

        result = (1, player1) if player1 else (2, player2)
        return result

    _, winning_deck = _play(player1, player2)
    return score(winning_deck)

print("Part B:", play_recursive_combat(player1, player2))
