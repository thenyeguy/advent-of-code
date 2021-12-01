import functools

class Seat(object):
    def __init__(self, partition):
        self.partition = partition

        row_partition = partition[:7]
        rowl, rowh = 0,127
        for p in row_partition:
            if p == "F":
                rowh = rowl + (rowh-rowl)//2
            elif p == "B":
                rowl = rowl + (rowh-rowl)//2 + 1
            else:
                raise Exception("Bad row partition: {}".format(row_partition))
        assert(rowl == rowh)
        self.row = rowl

        col_partition = partition[7:]
        coll, colh = 0,7
        for p in col_partition:
            if p == "L":
                colh = coll + (colh-coll)//2
            elif p == "R":
                coll = coll + (colh-coll)//2 + 1
            else:
                raise Exception("Bad col partition: {}".format(col_partition))
        assert(coll == colh)
        self.col = coll

        self.id = self.row * 8 + self.col

    def __str__(self):
        return "Seat({}, row={}, col={}, id={})".format(
                self.partition, self.row, self.col, self.id)


with open("data/5.txt", "r") as f:
    seats = [Seat(line.strip()) for line in f]

max_id = functools.reduce(max, (seat.id for seat in seats), 0)
print("Part A:", max_id)

last_id = None
for seat in sorted(seats, key=(lambda seat: seat.id)):
    if last_id and seat.id != last_id + 1:
        print("Part B:", seat.id-1)
        break
    last_id = seat.id
