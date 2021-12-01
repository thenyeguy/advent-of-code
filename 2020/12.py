import math

class Ship(object):
    def __init__(self):
        self.x = 0
        self.y = 0
        self.angle = 0

    def move(self, dx, dy):
        self.x += dx
        self.y += dy

    def advance(self, val):
        self.x += math.cos(self.angle) * val
        self.y += math.sin(self.angle) * val

    def rotate(self, rads):
        self.angle += rads

    def manhattan(self):
        return abs(self.x) + abs(self.y)

    def __str__(self):
        return "Ship({:.0f}, {:.0f}, {:.0f})".format(
                self.x, self.y, math.degrees(self.angle))

    def __repr__(self):
        return str(self)

class Waypoint(object):
    def __init__(self, x, y):
        self.ship = Ship()
        self.x = x
        self.y = y

    def move(self, dx, dy):
        self.x += dx
        self.y += dy

    def advance(self, val):
        dx = val * (self.x - self.ship.x)
        dy = val * (self.y - self.ship.y)
        self.ship.move(dx, dy)
        self.x += dx
        self.y += dy

    def rotate(self, rads):
        x = self.x - self.ship.x
        y = self.y - self.ship.y
        new_x = x*math.cos(rads) - y*math.sin(rads)
        new_y = y*math.cos(rads) + x*math.sin(rads)
        self.ship.rotate(rads)
        self.x = new_x + self.ship.x
        self.y = new_y + self.ship.y

    def __str__(self):
        rel_x = self.x - self.ship.x
        rel_y = self.y - self.ship.y
        return "Waypoint({}, {:.0f}, {:.0f})".format(self.ship, rel_x, rel_y)

    def __repr__(self):
        return str(self)

class Command(object):
    def __init__(self, cmd):
        self.action = cmd[0]
        self.value = int(cmd[1:])

    def apply(self, obj):
        if self.action == "N":
            obj.move(0, self.value)
        elif self.action == "S":
            obj.move(0, -self.value)
        elif self.action == "E":
            obj.move(self.value, 0)
        elif self.action == "W":
            obj.move(-self.value, 0)
        elif self.action == "L":
            obj.rotate(math.radians(self.value))
        elif self.action == "R":
            obj.rotate(-math.radians(self.value))
        elif self.action == "F":
            obj.advance(self.value)

    def __str__(self):
        return "{}({})".format(self.action, self.value)

    def __repr__(self):
        return str(self)

with open("data/12.txt", "r") as f:
    commands = list(Command(line.strip()) for line in f)


# Part A:
ship = Ship()
for command in commands:
    command.apply(ship)
print("Part A:", int(ship.manhattan()))


# Part B:
waypoint = Waypoint(10, 1)
for command in commands:
    command.apply(waypoint)
print("Part B:", int(waypoint.ship.manhattan()))
