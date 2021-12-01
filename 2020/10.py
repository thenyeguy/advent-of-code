import collections
import functools
import operator


with open("data/10.txt", "r") as f:
    data = [int(line) for line in f]

data.append(0)  # wall adapter
data.sort()
data.append(data[-1] + 3)  # internal charger


# Part A:
diffs = [data[i] - data[i-1] for i in range(1, len(data))]
print("Part A:", diffs.count(1) * diffs.count(3))


# Part B:
runs = collections.defaultdict(int)
run = 0
for diff in diffs + [None]:
    if diff == 1:
        run += 1
    elif run > 0:
        runs[run] += 1
        run = 0

def permutations(run_length):
    num_permutations = [None, 1, 2, 4, 7]
    return num_permutations[run_length]

num_permutations = functools.reduce(
    operator.mul,
    (pow(permutations(length), count) for (length, count) in runs.items()),
    1
)
print("Part B:", num_permutations)
