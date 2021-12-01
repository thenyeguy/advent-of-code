with open("data/9.txt", "r") as f:
    data = [int(line) for line in f]

def validate(data, idx):
    assert(idx >= 25)
    for i in range(idx-25, idx):
        for j in range(idx-25, idx):
            if i != j and data[i]+data[j]==data[idx]:
                return (i,j)

for idx in range(25, len(data)):
    if not validate(data, idx):
        first_invalid = idx
        break

print("Part A:", data[idx])


def find_sum_range(data, target):
    lower = 0
    upper = 0
    running_sum = data[0]
    while running_sum != target:
        if running_sum < target:
            upper += 1
            running_sum += data[upper]
        elif running_sum > target:
            running_sum -= data[lower]
            lower += 1
    return (lower, upper)

def find_weakness(data):
    i, j = find_sum_range(data, data[idx])
    slice = data[i:j]
    return min(slice) + max(slice)

weakness = find_weakness(data)
print("Part B:", weakness)
