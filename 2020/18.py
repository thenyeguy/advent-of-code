import operator
import re


class ReversePolishNotation(object):
    def __init__(self, terms):
        self.terms = []
        for term in terms:
            if re.match("\d+", term):
                self.terms.append(int(term))
            elif term == "+":
                self.terms.append(operator.add)
            elif term == "*":
                self.terms.append(operator.mul)

    @classmethod
    def parse(obj, expr, operator_precedence):
        expr = "( " + expr.replace("(", "( ").replace(")", " )") + " )"
        rpn = []
        op_stack = []
        for term in expr.split(" "):
            if re.match("\d+", term):
                rpn.append(term)
            elif term in operator_precedence:
                precedence = operator_precedence[term]
                while (op_stack and op_stack[-1] != "(" and
                       operator_precedence[op_stack[-1]] >= precedence):
                    rpn.append(op_stack.pop())
                op_stack.append(term)
            elif term == "(":
                op_stack.append("(")
            elif term == ")":
                while op_stack[-1] != "(":
                    rpn.append(op_stack.pop())
                op_stack.pop()
            else:
                raise ValueError("Unknown term: " + term)
        assert(len(op_stack) == 0)
        return obj(rpn)

    def evaluate(self):
        stack = []
        for term in self.terms:
            if isinstance(term, int):
                stack.append(term)
            else:
                a = stack.pop()
                b = stack.pop()
                stack.append(term(a, b))
        assert(len(stack) == 1)
        return stack.pop()

    def __repr__(self):
        return str(self)

    def __str__(self):
        def _term_to_str(term):
            if isinstance(term, int):
                return str(term)
            elif term.__name__ == "add":
                return "+"
            elif term.__name__ == "mul":
                return "*"
            else:
                raise Exception()
        return "RPN(" + " ".join(_term_to_str(term) for term in self.terms) + ")"


with open("data/18.txt", "r") as f:
    expressions = [line.strip() for line in f]


# Part A:
result = sum(ReversePolishNotation.parse(expr, {"+": 1, "*": 1}).evaluate()
             for expr in expressions)
print("Part A:", result)


# Part B:
result = sum(ReversePolishNotation.parse(expr, {"+": 2, "*": 1}).evaluate()
             for expr in expressions)
print("Part B:", result)
