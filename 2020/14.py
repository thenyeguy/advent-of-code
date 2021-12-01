import re


class Mask(object):
    def __init__(self, mask):
        self.mask = mask

    def apply_a(self, value):
        and_mask = int(self.mask.replace("X", "1"), base=2)
        or_mask = int(self.mask.replace("X", "0"), base=2)
        return value & and_mask | or_mask

    def apply_b(self, address):
        or_mask = int(self.mask.replace("X", "0"), base=2)
        addresses = [address | or_mask]
        for i in range(len(self.mask)):
            if self.mask[-(i+1)] == "X":
                bit = 1 << i
                new_addresses = []
                for address in addresses:
                    new_addresses.append(address | bit)
                    new_addresses.append(address & ~bit)
                addresses = new_addresses
        return addresses

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "Mask({})".format(self.mask)

class Memory(object):
    def __init__(self, address, value):
        self.address = int(address)
        self.value = int(value)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "mem[{}] = {}".format(self.address, self.value)


with open("data/14.txt", "r") as f:
    instructions = []
    for line in f:
        mask = re.match(r"mask = (\w+)", line)
        memory = re.match(r"mem\[(\d+)\] = (\d+)", line)
        if mask:
            instructions.append(Mask(mask.group(1)))
        elif memory:
            instructions.append(Memory(memory.group(1), memory.group(2)))
        else:
            raise Exception("Unparsed instruction: " + line.strip())


# Part A:
mask = None
memory = {}
for instruction in instructions:
    if isinstance(instruction, Mask):
        mask = instruction
    elif isinstance(instruction, Memory):
        write = instruction
        memory[write.address] = mask.apply_a(write.value)
print("Part A: {}".format(sum(memory.values())))


# Part B:
mask = None
memory = {}
for instruction in instructions:
    if isinstance(instruction, Mask):
        mask = instruction
    elif isinstance(instruction, Memory):
        write = instruction
        for address in mask.apply_b(write.address):
            memory[address] = write.value
print("Part B: {}".format(sum(memory.values())))
