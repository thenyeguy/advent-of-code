import copy

class SeatMap(object):
    def __init__(self, map):
        self.map = map
        self.rows = len(map)
        self.cols = len(map[0])
        self.ticks = 0

    def tick(self, update_rule):
        new_map = copy.deepcopy(self.map)
        for i in range(self.rows):
            for j in range(self.cols):
                if self.get(i, j) == ".":
                    continue
                new_seat = update_rule(self, i, j)
                if new_seat:
                    new_map[i][j] = new_seat
        changed = self.map != new_map
        self.ticks += 1
        self.map = new_map
        return changed

    def get(self, i, j):
        if i < 0 or i >= self.rows or j < 0 or j >= self.cols:
            return None
        return self.map[i][j]

    def get_visible(self, i, j, di, dj):
        seat = "."
        while seat and seat == ".":
            i += di
            j += dj
            seat = self.get(i, j)
        return seat

    def count_occupied_seats(self):
        return sum(1 if self.map[i][j] == "#" else 0
                   for i in range(self.rows)
                   for j in range(self.cols))

    def __str__(self):
        return "\n".join("".join(row) for row in self.map)

with open("data/11.txt", "r") as f:
    seats = SeatMap([list(line.strip()) for line in f])


# Rule A:
def rule_a(seats, i, j):
    seat = seats.get(i, j)
    adjacent_seats = sum(1 if seats.get(ii,jj) == "#" else 0
                         for ii in (i-1, i, i+1)
                         for jj in (j-1, j, j+1)
                         if ii != i or jj != j)

    if seat == "L" and adjacent_seats == 0:
        return "#"
    elif seat == "#" and adjacent_seats >= 4:
        return "L"

seats_a = copy.deepcopy(seats)
while seats_a.tick(rule_a): pass
print("Part A:", seats_a.count_occupied_seats())


# Rule B:
def rule_b(seats, i, j):
    seat = seats.get(i, j)
    visible_seats = sum(1 if seats.get_visible(i, j, di, dj) == "#" else 0
                        for di in (-1, 0, 1)
                        for dj in (-1, 0, 1)
                        if di != 0 or dj != 0)
    if seat == "L" and visible_seats == 0:
        return "#"
    elif seat == "#" and visible_seats >= 5:
        return "L"

seats_b = copy.deepcopy(seats)
while seats_b.tick(rule_b): pass
print("Part B:", seats_b.count_occupied_seats())
