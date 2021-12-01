import collections
import re

# Parse out edge rules
with open("data/7.txt", "r") as f:
    rules = {}
    for line in f:
        match = re.match(r"(.+) bags contain (.+)\.", line)
        outer = match.group(1)
        inners = match.group(2)

        if inners == "no other bags":
            rules[outer] = {}
        else:
            inner_counts = {}
            for bag in inners.split(", "):
                match = re.match(r"(\d+) (.+) bags?", bag)
                inner_counts[match.group(2)] = int(match.group(1))
            rules[outer] = inner_counts

# Build a reverse directed graph
contained_in = collections.defaultdict(set)
for (key, value) in rules.items():
    for contains in value:
        contained_in[contains].add(key)

# Finds all bags that can contain `bag`, directly or transitively.
# Note: assumes no cycles in the data.
def can_contain(bag):
    containers = set()
    for container in contained_in[bag]:
        containers.add(container)
        containers.update(can_contain(container))
    return containers

print("Part A:", len(can_contain("shiny gold")))

# Finds how many bags must be inside `bag`.
def num_contained(bag):
    bags = 0
    for (contained, count) in rules[bag].items():
        bags += count * (1 + num_contained(contained))
    return bags

print("Part B:", num_contained("shiny gold"))
