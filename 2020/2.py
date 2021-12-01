import re

class PasswordPolicy(object):
    def __init__(self, letter, lower, upper):
        self.letter = letter
        self.lower = lower
        self.upper = upper

    def validate_a(self, password):
        count = 0
        for c in password:
            if c == self.letter:
                count += 1
        return self.lower <= count <= self.upper

    def validate_b(self, password):
        return ((password[self.lower - 1] == self.letter) ^
                (password[self.upper - 1] == self.letter))


with open("data/2.txt", "r") as f:
    passwords = []
    for line in f:
        match = re.match(r"(\d+)-(\d+) (\w): (\w+)", line.strip())
        policy = PasswordPolicy(
            match.group(3), int(match.group(1)), int(match.group(2)))
        password = match.group(4)
        passwords.append((policy, password))


# Part A:
valid_passwords = 0
for (policy, password) in passwords:
    if policy.validate_a(password):
        valid_passwords += 1
print("Part A:", valid_passwords)


# Part B:
valid_passwords = 0
for (policy, password) in passwords:
    if policy.validate_b(password):
        valid_passwords += 1
print("Part B:", valid_passwords)
