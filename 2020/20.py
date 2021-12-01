import collections

def rotate(data):
    return [[data[-(j+1)][i] for j in range(len(data))]
                             for i in range(len(data[0]))]

def count(data, needle):
    return sum(1 for row in data for item in row if item == needle)


class Image(object):
    def __init__(self, id, data):
        self.id = id
        self.data = data

    def rotate(self):
        return Image(self.id, rotate(self.data))

    def flip(self):
        return Image(self.id, [row[::-1] for row in self.data])

    def count(self, pattern):
        def contains_pattern(row, col):
            for dr in range(0, len(pattern)):
                for dc in range(0, len(pattern[0])):
                    if (pattern[dr][dc] == "#" and
                            self.data[row + dr][col + dc] != "#"):
                        return False
            return True


        count = 0
        for row in range(0, len(self.data) - len(pattern)):
            for col in range(0, len(self.data[0]) - len(pattern[0])):
                if contains_pattern(row, col):
                    count += 1
        return count

    def top(self):
        top = "".join(self.data[0])
        return min(top, top[::-1])

    def bottom(self):
        bottom = "".join(self.data[-1])
        return min(bottom, bottom[::-1])

    def left(self):
        left = "".join(row[0] for row in self.data)
        return min(left, left[::-1])

    def right(self):
        right = "".join(row[-1] for row in self.data)
        return min(right, right[::-1])

    def edges(self):
        return [self.top(), self.right(), self.bottom(), self.left()]

    def __repr__(self):
        return "{}".format(self.id)

    def __str__(self):
        return "{}:\n{}\n".format(
                self.id, "\n".join(" ".join(row) for row in self.data))


with open("data/20.txt", "r") as f:
    tiles = []
    while True:
        line = f.readline()
        if not line: break
        id = int(line[5:9])
        line = f.readline().strip()
        data = []
        while line:
            data.append(list(line))
            line = f.readline().strip()
        tiles.append(Image(id, data))


# Part A:
# Find all possible edges:
edges = collections.defaultdict(set)
for tile in tiles:
    for edge in tile.edges():
        edges[edge].add(tile)

# Find how many edges each image shares.
shared_edges = collections.defaultdict(int)
for edge, _tiles in edges.items():
    if len(_tiles) > 1:
        for tile in _tiles:
            shared_edges[tile] += 1

# Split into corner, edge, and center tiles:
# This works because there seems to be only one correct alignment of each image.
corner_tiles = []
for tile, edge_count in shared_edges.items():
    if edge_count == 2:
        corner_tiles.append(tile)
assert len(corner_tiles) == 4

product = 1
for tile in corner_tiles:
    product *= tile.id
print("Part A:", product)


# Part B:
# Create an empty tile grid, and seed with a corner (aligned correctly).
image_tiles = [[None for _ in range(12)] for _ in range(12)]
seed = corner_tiles[0]
while [len(edges[edge]) for edge in seed.edges()] != [1, 2, 2, 1]:
    seed = seed.rotate()
used_tiles = {seed.id}
image_tiles[0][0] = seed

# Fill the left edge top to bottom, then rotate and fill all columns:
def fill_col(image_tiles, col):
    for row in range(0, len(image_tiles)-1):
        top = image_tiles[row][col]
        bottom = None
        for tile in edges[top.bottom()]:
            if tile.id != top.id:
                bottom = tile
                break
        assert bottom
        while bottom.top() != top.bottom():
            bottom = bottom.rotate()
        if top.data[-1] != bottom.data[0]:
            bottom = bottom.flip()
        image_tiles[row+1][col] = bottom

fill_col(image_tiles, 0)
for row in image_tiles:
    row[0] = row[0].rotate()
image_tiles = rotate(image_tiles)

for col in range(len(image_tiles[0])):
    fill_col(image_tiles, col)


# Stitch together all the tiles into a single image:
def trim_border(data):
    return [[data[i][j] for j in range(1, len(data[0])-1)]
                        for i in range(1, len(data)-1)]

image_data = []
for image_row in image_tiles:
    row_tiles = [trim_border(tile.data) for tile in image_row]
    num_rows = len(row_tiles[0])
    rows = [[] for _ in range(num_rows)]
    for tile in row_tiles:
        for row in range(num_rows):
            rows[row].extend(tile[row])
    image_data.extend(rows)
full_image = Image("FullImage", image_data)

# Find the sea monsters!
sea_monster = [
    list("                  # "),
    list("#    ##    ##    ###"),
    list(" #  #  #  #  #  #   "),
]

for _ in range(4):
    sea_monsters = full_image.count(sea_monster)
    if sea_monsters: break
    full_image = full_image.flip()
    sea_monsters = full_image.count(sea_monster)
    if sea_monsters: break
    full_image = full_image.rotate()

choppy_seas = (count(full_image.data, "#") -
               sea_monsters * count(sea_monster, "#"))
print("Part B:", choppy_seas)
