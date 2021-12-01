with open("data/1.txt", "r") as f:
    nums = [int(line) for line in f.readlines()]

# Part B:
def run_a(nums):
    for a in nums:
        for b in nums:
            if a + b == 2020:
                return a * b
    raise Exception("No pair found")
print("Part A:", run_a(nums))


# Part B:
def run_b(nums):
    for a in nums:
        for b in nums:
            for c in nums:
                if a + b + c == 2020:
                    return a * b * c
    raise Exception("No triple found")
print("Part B:", run_b(nums))
