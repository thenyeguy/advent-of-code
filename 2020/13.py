import math

with open("data/13.txt", "r") as f:
    timestamp = int(f.readline())
    busses = list(int(id) if id.isdigit() else None
                  for id in f.readline().strip().split(","))

# Part A:
def time_to_next_bus(timestamp, bus):
    return bus - timestamp % bus

running_busses = [(bus, time_to_next_bus(timestamp, bus))
                  for bus in busses if bus]
next_bus = min(running_busses, key=lambda bus: bus[1])
print("Part A:", next_bus[0] * next_bus[1])


# Part B:
#
# time = -i   mod bus
#   for (bus, i) in bus_offsets
#
# Compute using Chinese Remainder Theorem
class Congruence(object):
    def __init__(self, base, remainder):
        self.base = base
        self.remainder = remainder % base

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "{} % {}".format(self.remainder, self.base)

def sieve(congruences):
    congruences.sort(key=lambda c: c.base, reverse=True)
    x = congruences[0].remainder
    n = congruences[0].base
    for c in congruences[1:]:
        while (x % c.base) != c.remainder:
            x += n
        n *= c.base
    x = x % n
    return x

bus_congruences = [Congruence(bus, -i) for (i, bus) in enumerate(busses) if bus]
result = sieve(bus_congruences)
print("Part B:", result)
