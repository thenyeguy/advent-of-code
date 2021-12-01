import copy
import re


class Instruction(object):
    def __init__(self, text):
        name, arg = re.match(r"(\w+) (.+)", text).groups()
        self.name = name
        self.arg = int(arg)

    def __str__(self):
        return "{} {}".format(self.name, self.arg)

class ProgramState(object):
    def __init__(self):
        self.acc = 0
        self.pc = 0

    def tick(self, program):
        if self.pc == len(program):
            return True

        instruction = program[self.pc]
        if instruction.name == "jmp":
            self.pc += instruction.arg
        elif instruction.name == "acc":
            self.acc += instruction.arg
            self.pc += 1
        else:
            self.pc += 1
        return False


with open("data/8.txt", "r") as f:
    program = [Instruction(line.strip()) for line in f]


def run(program, starting_pc=0):
    state = ProgramState()
    state.pc = starting_pc
    reachable = set()
    while state.pc not in reachable:
        reachable.add(state.pc)
        if state.tick(program):
            break
    return (state.acc, reachable)

acc, reachable = run(program)
print("Part A:", acc)


# Brute force simulation:
def reached_end(program, reachable):
    return len(program) - 1 in reachable

def fix_bad_instruction(program, reachable):
    reaches_end = set()
    for pc in range(len(program)):
        if pc in reachable:
            continue
        _, new_reachable = run(program, pc)
        if reached_end(program, new_reachable):
            reaches_end.add(pc)

    fixed_pc = None
    for pc in reachable:
        instruction = program[pc]
        if instruction.name == "jmp":
            if pc + 1 in reaches_end:
                fixed_pc = pc
                break
        elif instruction.name == "nop":
            if pc + instruction.arg in reaches_end:
                fixed_pc = pc
                break

    fixed_program = copy.deepcopy(program)
    instruction = fixed_program[fixed_pc]
    if instruction.name == "jmp":
        instruction.name = "nop"
    elif instruction.name == "nop":
        instruction.name = "jmp"
    return fixed_program

fixed_program = fix_bad_instruction(program, reachable)
acc, fixed_reachable = run(fixed_program)
assert(reached_end(fixed_program, fixed_reachable))
print("Part B:", acc)
