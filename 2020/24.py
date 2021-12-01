import collections
import re

with open("data/24.txt", "r") as f:
    directions = [line.strip() for line in f]


# Part A:
def get_tile(directions):
    tile = (0, 0)
    for dir in re.findall(r"(w|e|sw|se|nw|ne)", directions):
        tile = {
            "w":  (tile[0]-1, tile[1]),
            "e":  (tile[0]+1, tile[1]),
            "sw": (tile[0],   tile[1]-1),
            "se": (tile[0]+1, tile[1]-1),
            "nw": (tile[0]-1, tile[1]+1),
            "ne": (tile[0],   tile[1]+1),
        }[dir]
    return tile

black_tiles = set()
for direction in directions:
    tile = get_tile(direction)
    if tile in black_tiles:
        black_tiles.remove(tile)
    else:
        black_tiles.add(tile)
print("Part A:", len(black_tiles))


# Part B:
def get_adjacent_tiles(tile):
    return {
        (tile[0]-1, tile[1]+1),
        (tile[0],   tile[1]+1),
        (tile[0]-1, tile[1]),
        (tile[0]+1, tile[1]),
        (tile[0],   tile[1]-1),
        (tile[0]+1, tile[1]-1),
    }

def flip_tiles(black_tiles):
    adjacent_tiles = collections.defaultdict(int)
    for black_tile in black_tiles:
        for tile in get_adjacent_tiles(black_tile):
            adjacent_tiles[tile] += 1

    new_black_tiles = set()
    for tile, count in adjacent_tiles.items():
        if tile in black_tiles:
            if count in (1, 2):
                new_black_tiles.add(tile)
        else:
            if count == 2:
                new_black_tiles.add(tile)
    return new_black_tiles

for _ in range(100):
    black_tiles = flip_tiles(black_tiles)
print("Part B:", len(black_tiles))
