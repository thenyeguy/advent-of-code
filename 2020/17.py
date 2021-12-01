seed = [
    "##......",
    ".##...#.",
    ".#######",
    "..###.##",
    ".#.###..",
    "..#.####",
    "##.####.",
    "##..#.##",
]

# Part A:
class PocketDimension3(object):
    def __init__(self, seed):
        self.map = [[list(row) for row in seed]]

    def tick(self):
        sx = len(self.map[0][0]) + 2
        sy = len(self.map[0]) + 2
        sz = len(self.map) + 2
        new_map = [[["." for _ in range(sx)]
                         for _ in range(sy)]
                         for _ in range(sz)]

        for x in range(0, sx):
            for y in range(0, sy):
                for z in range(0, sz):
                    cell = self.get(x-1, y-1, z-1)
                    neighbors = self.count_neighbors(x-1, y-1, z-1)
                    if cell == "#" and neighbors not in (2, 3):
                        cell = "."
                    elif cell == "." and neighbors == 3:
                        cell = "#"
                    new_map[z][y][x] = cell
        self.map = new_map

    def count_neighbors(self, x, y, z):
        neighbors = 0
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                for dz in (-1, 0, 1):
                    if (dx, dy, dz) == (0, 0, 0):
                        continue
                    if self.get(x+dx, y+dy, z+dz) == "#":
                        neighbors += 1
        return neighbors

    def get(self, x, y, z):
        sx = len(self.map[0][0])
        sy = len(self.map[0])
        sz = len(self.map)
        if x < 0 or x >= sx:
            return "."
        if y < 0 or y >= sy:
            return "."
        if z < 0 or z >= sz:
            return "."
        return self.map[z][y][x]

    def count_active_cells(self):
        return sum(sum(sum(1 for cell in row if cell == "#")
                             for row in plane)
                             for plane in self.map)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return "\n\n".join("\n".join("".join(row) for row in plane)
                                                  for plane in self.map)

dimension = PocketDimension3(seed)
for _ in range(6):
    dimension.tick()
print("Part A:", dimension.count_active_cells())


# Part B:
class PocketDimension4(object):
    def __init__(self, seed):
        self.map = [[[list(row) for row in seed]]]

    def tick(self):
        sx = len(self.map[0][0][0]) + 2
        sy = len(self.map[0][0]) + 2
        sz = len(self.map[0]) + 2
        sw = len(self.map) + 2
        new_map = [[[["." for _ in range(sx)]
                          for _ in range(sy)]
                          for _ in range(sz)]
                          for _ in range(sw)]

        for x in range(0, sx):
            for y in range(0, sy):
                for z in range(0, sz):
                    for w in range(0, sw):
                        cell = self.get(x-1, y-1, z-1, w-1)
                        neighbors = self.count_neighbors(x-1, y-1, z-1, w-1)
                        if cell == "#" and neighbors not in (2, 3):
                            cell = "."
                        elif cell == "." and neighbors == 3:
                            cell = "#"
                        new_map[w][z][y][x] = cell
        self.map = new_map

    def count_neighbors(self, x, y, z, w):
        neighbors = 0
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                for dz in (-1, 0, 1):
                    for dw in (-1, 0, 1):
                        if (dx, dy, dz, dw) == (0, 0, 0, 0):
                            continue
                        if self.get(x+dx, y+dy, z+dz, w+dw) == "#":
                            neighbors += 1
        return neighbors

    def get(self, x, y, z, w):
        sx = len(self.map[0][0][0])
        sy = len(self.map[0][0])
        sz = len(self.map[0])
        sw = len(self.map)
        if x < 0 or x >= sx:
            return "."
        if y < 0 or y >= sy:
            return "."
        if z < 0 or z >= sz:
            return "."
        if w < 0 or w >= sw:
            return "."
        return self.map[w][z][y][x]

    def count_active_cells(self):
        return sum(sum(sum(sum(1 for cell in row if cell == "#")
                                 for row in plane)
                                 for plane in cube)
                                 for cube in self.map)

dimension = PocketDimension4(seed)
for _ in range(6):
    dimension.tick()
print("Part B:", dimension.count_active_cells())
