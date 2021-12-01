seed = [3, 6, 8, 1, 9, 5, 7, 4, 2]


class Cups(object):
    def __init__(self, seed, size=None):
        self.start = seed[0]
        self.next_cup = [None] + [i+2 for i in range(size or len(seed))]
        self.length = len(self.next_cup) - 1
        for i in range(len(seed) - 1):
            cup = seed[i]
            next = seed[i+1]
            self.next_cup[cup] = next
        if size:
            self.next_cup[seed[-1]] = len(seed)+1
            self.next_cup[-1] = seed[0]
        else:
            self.next_cup[seed[-1]] = seed[0]

    def set_next(self, cup, next):
        self.next_cup[cup] = next

    def slice_after(self, start, count):
        slice = [None] * count
        current = start
        for i in range(count):
            current = self.next_cup[current]
            slice[i] = current
        return slice

    def to_list(self):
        result = [0] * self.length
        current = self.start
        for i in range(self.length):
            result[i] = current
            current = self.next_cup[current]
        assert current == self.start
        return result
        
    def play(self, moves=100):
        current = self.start
        picked_up = [None] * 3
        for _ in range(moves):
            picked_up[0] = self.next_cup[current]
            picked_up[1] = self.next_cup[picked_up[0]]
            picked_up[2] = self.next_cup[picked_up[1]]

            destination = current - 1
            while destination < 1 or destination in picked_up:
                if destination < 1:
                    destination = len(self.next_cup) - 1
                else:
                    destination -= 1

            self.next_cup[current] = self.next_cup[picked_up[-1]]
            self.next_cup[picked_up[-1]] = self.next_cup[destination]
            self.next_cup[destination] = picked_up[0]
            current = self.next_cup[current]


# Part A:
def score_a(cups):
    return "".join(str(c) for c in cups.slice_after(1, 8))

cups_a = Cups(seed)
cups_a.play()
print("Part A:", score_a(cups_a))


# Part B:
def score_b(cups):
    [a, b] = cups.slice_after(1, 2)
    return a * b

cups_b = Cups(seed, 1000000)
cups_b.play(10000000)
print("Part B:", score_b(cups_b))
