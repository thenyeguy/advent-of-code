import re


class Food(object):
    def __init__(self, description):
        match = re.match(r"(.*) \(contains (.*)\)", description)
        self.ingredients = set(match.group(1).split(" "))
        self.allergens = set(match.group(2).split(", "))

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "{} (contains {})".format(
            " ".join(self.ingredients), ", ".join(self.allergens))


with open("data/21.txt", "r") as f:
    foods = [Food(line) for line in f]

# Part A:
possible_allergens = dict()
possible_ingredients = set()
for food in foods:
    for allergen in food.allergens:
        possible_ingredients.update(food.ingredients)
        if allergen in possible_allergens:
            possible_allergens[allergen].intersection_update(food.ingredients)
        else:
            possible_allergens[allergen] = set(food.ingredients)

dangerious_ingredients = set(ingredient
                             for ingredients in possible_allergens.values()
                             for ingredient in ingredients)
safe_ingredients = possible_ingredients - dangerious_ingredients

num_safe_ingredients = sum(len(safe_ingredients.intersection(food.ingredients))
                           for food in foods)
print("Part A:", num_safe_ingredients)


# Part B:
allergens = dict()
num_allergens = len(possible_allergens)
while len(allergens)  < num_allergens:
    for allergen, possibles in possible_allergens.items():
        if len(possibles) == 1:
            ingredient = possibles.pop()
            allergens[allergen] = ingredient
            del possible_allergens[allergen]
            for possibles in possible_allergens.values():
                possibles.discard(ingredient)
            break

canonical_list = ",".join(allergens[a] for a in sorted(allergens.keys()))
print("Part B:", canonical_list)
