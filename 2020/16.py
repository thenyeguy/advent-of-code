class Range(object):
    def __init__(self, lower, upper):
        self.lower = int(lower)
        self.upper = int(upper)

    def validate(self, n):
        return self.lower <= n <= self.upper

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "[{},{}]".format(self.lower, self.upper)

class Validator(object):
    def __init__(self, line):
        self.name = line.split(":")[0]
        self.ranges = []
        for range in line.split(":")[1].split(" "):
            if "-" in range:
                self.ranges.append(Range(*range.split("-")))

    def validate(self, n):
        return any(range.validate(n) for range in self.ranges)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "{}: {}".format(self.name, self.ranges)


with open("data/16.txt", "r") as f:
    validators = []
    while True:
        line = f.readline().strip()
        if line:
            validators.append(Validator(line))
        else:
            break

    f.readline()
    my_ticket = [int(n) for n in f.readline().split(",")]

    f.readline()
    f.readline()
    nearby_tickets = []
    for line in f:
        ticket = [int(n) for n in line.split(",")]
        nearby_tickets.append(ticket)


# Part A:
def invalid_fields(validators, ticket):
    return [field for field in ticket
                  if not any(validator.validate(field)
                             for validator in validators)]

result = sum(field for ticket in nearby_tickets
                   for field in invalid_fields(validators, ticket))
print("Part A:", result)


# Part B:
def validate_ticket(validators, ticket):
    return all(any(validator.validate(field) for validator in validators)
                                             for field in ticket)
valid_tickets = [ticket for ticket in nearby_tickets
                        if validate_ticket(validators, ticket)]

num_fields = len(my_ticket)
valid_indices = {validator.name: set(range(num_fields))
                 for validator in validators}

for field in range(num_fields):
    for ticket in valid_tickets:
        for validator in validators:
            if not validator.validate(ticket[field]):
                valid_indices[validator.name].remove(field)

# This is cheesy, but it turns out that just handling these in order always
# eliminates all but one key from the next field. Does not generalize.
fields = dict()
assigned_indices = set()
for field in sorted(valid_indices, key=lambda f: len(valid_indices[f])):
    valid_indices[field] -= assigned_indices
    assert len(valid_indices[field]) == 1
    index = list(valid_indices[field])[0]
    fields[field] = index
    assigned_indices.add(index)

result = 1
for field, idx in fields.items():
    if field.startswith("departure"):
        result *= my_ticket[idx]
print("Part B:", result)
