with open("data/19.txt", "r") as f:
    rules = []
    line = f.readline().strip()
    while line:
        rules.append(line)
        line = f.readline().strip()
    messages = [line.strip() for line in f]


class Rules(object):
    class Literal(object):
        def __init__(self, char):
            self.char = char

        def match(self, rules, s):
            if s == "":
                raise ValueError("No string left")
            elif s[0] == self.char:
                return s[1:]
            else:
                raise ValueError("Bad char");

        def __str__(self):
            return '"{}"'.format(self.char)

    class And(object):
        def __init__(self, rules):
            self.rules = rules

        def match(self, rules, s):
            for rule in self.rules:
                s = rules[rule].match(rules, s)
            return s

        def __str__(self):
            return " ".join(self.rules)

    class Or(object):
        def __init__(self, rules):
            self.rules = rules

        def match(self, rules, s):
            for rule in self.rules:
                try:
                    return rule.match(rules, s)
                except ValueError:
                    pass
            raise ValueError("No rule matched Or")

        def __str__(self):
            return " | ".join(str(rule) for rule in self.rules)

    def __init__(self, rules):
        self.rules = {}
        for rule in rules:
            idx, payload = rule.split(": ")
            if payload.startswith('"'):
                self.rules[idx] = Rules.Literal(payload[1])
            elif "|" in payload:
                self.rules[idx] = Rules.Or(
                    [Rules.And(ors.split(" ")) for ors in payload.split(" | ")])
            else:
                self.rules[idx] = Rules.And(payload.split(" "))

    def match(self, s):
        try:
            s = self.rules["0"].match(self.rules, s)
            if s:
                raise ValueError("Didn't match whole string")
        except ValueError:
            return False
        return True

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "\n".join("{}: {}".format(idx, rule)
                         for (idx, rule) in self.rules.items())


# Part A:
rules_a = Rules(rules)
matching = sum([1 for message in messages if rules_a.match(message)])
print("Part A:", matching)


# Part B:
# 
# The new rules 8 and 11 are recursive. However, only one rule references them:
#   0:  8  11
#   8:  42    | 42 8
#   11: 42 31 | 42 11 31
#
# We can work around this specific case by allowing rule 11 to recurse normally,
# and adding special logic to read the rule 8 head:
class RecursiveRule(object):
    def match(self, rules, s):
        while True:
            # Test rule 8.
            #   If it matches, test rule 11.
            #   If not, we'll throw, as we didn't find a match.
            s = rules["8"].match(rules, s)
            try:
                # Test rule 11 on the remainder.
                #   If it matches, we're done.
                #   If not, pass the reaminder back to rule 8.
                s = rules["11"].match(rules, s)
                return s
            except ValueError:
                continue
        return s

    def __str__(self):
        return "** 8 11 **"

rules_b = Rules(rules + ["11: 42 31 | 42 11 31"])
rules_b.rules["0"] = RecursiveRule()
matching = sum([1 for message in messages if rules_b.match(message)])
print("Part B:", matching)
