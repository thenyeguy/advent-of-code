with open("data/3.txt", "r") as f:
    map = [line.strip() for line in f]


def count_trees(map, dx, dy=1):
    x = 0
    trees = 0
    for line in map[::dy]:
        if line[x % len(line)] == "#":
            trees += 1
        x += dx
    return trees

# Part A:
print("Part A:", count_trees(map, 3))


# Part B:
total = 1
for (dx, dy) in [(1,1), (3,1), (5,1), (7,1), (1,2)]:
    trees = count_trees(map, dx, dy)
    total = total * trees
print("Part B:", total)
