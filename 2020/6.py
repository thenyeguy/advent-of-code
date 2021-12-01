with open("data/6.txt", "r") as f:
    groups = []
    group = []
    for line in f:
        line = line.strip()
        if line:
            group.append(line)
        else:
            groups.append(group)
            group = []

unions = 0
for group in groups:
    yeses = set()
    for person in group:
        yeses.update(set(person))
    unions += len(yeses)
print("Part A:", unions)

intersections = 0
for group in groups:
    yeses = set(group[0])
    for person in group:
        yeses.intersection_update(set(person))
    intersections += len(yeses)
print("Part B:", intersections)
