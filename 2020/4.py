import re

passports = []

with open("data/4.txt", "r") as f:
    passport = {}
    for line in f:
        line = line.strip()
        if line:
            for field in line.split(" "):
                (key, value) = field.split(":")
                passport[key] = value
        else:
            passports.append(passport)
            passport = {}
    passports.append(passport)


# Part A:
def validate_a(passport):
    valid_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    for field in valid_fields:
        if field not in passport:
            return False
    return True

valid_passports = 0
for passport in passports:
    if validate_a(passport):
        valid_passports += 1
print("Part A:", valid_passports)

# Part B:
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#     If cm, the number must be at least 150 and at most 193.
#     If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
def validate_b(passport):
    try:
        byr = int(passport["byr"])
        if byr < 1920 or byr > 2002:
            return False

        iyr = int(passport["iyr"])
        if iyr < 2010 or iyr > 2020:
            return False

        eyr = int(passport["eyr"])
        if eyr < 2020 or eyr > 2030:
            return False

        hgt = passport["hgt"]
        hgt_match = re.match(r"(\d+)(cm|in)", hgt)
        dim = int(hgt_match.group(1))
        unit = hgt_match.group(2)
        if unit == "cm" and (dim < 150 or dim > 193):
            return False
        elif unit == "in" and (dim <  59 or dim > 76):
            return False

        hcl = passport["hcl"]
        if not re.fullmatch(r"#[a-f0-9]{6}", hcl):
            return False

        ecl = passport["ecl"]
        if ecl not in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"):
            return False

        pid = passport["pid"]
        if not re.fullmatch(r"\d{9}", pid):
            return False
    except Exception as e:
        return False
    return True

valid_passports = 0
for passport in passports:
    if validate_b(passport):
        valid_passports += 1
print("Part B:", valid_passports)
