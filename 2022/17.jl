module Day17

function load_data()
    return read(joinpath(@__DIR__, "data", "17.txt"), String)
end

const ROCKS::Array{Matrix{Bool}} = [
    [1 1 1 1],             # _ rock
    [0 1 0; 1 1 1; 0 1 0], # + rock
    [1 1 1; 0 0 1; 0 0 1], # L rock
    [1;1;1;1;;],           # | rock
    [1 1;1 1],             # o rock
]

function check_collision(cave, rock, bottom_left)
    top_right = bottom_left + CartesianIndex(size(rock)) - CartesianIndex(1,1)
    return any(cave[bottom_left:top_right] .* rock)
end

function add_rock!(cave, rock, pos)
    bottom_left = pos + CartesianIndex(1,0)
    top_right = bottom_left + CartesianIndex(size(rock)) - CartesianIndex(1,1)
    cave[bottom_left:top_right] .|= rock
end

function draw_cave(cave)
    for row in size(cave, 1):-1:2
        print("|")
        for col in 2:(size(cave, 2)-1)
            print(cave[row,col] ? "#" : ".")
        end
        println("| ", row - 1)
    end
    println("+-------+")
end

function drop_rocks(jets, num_rocks, detect_cycles=false)
    height = 1
    total_rocks = 0
    next_rock_idx = 1
    next_jet_idx = 1

    pos = nothing
    rock = nothing
    seen = Dict()

    cave = ones(Bool, 1, 9)
    new_chunk = hcat(ones(Bool, 7, 1), zeros(Bool, 7, 7), ones(Bool, 7, 1))

    while total_rocks < num_rocks
        # Spawn a new rock
        if isnothing(rock)
            if size(cave, 1) < height + 7
                cave = vcat(cave, new_chunk)
            end
            pos = CartesianIndex(height+4, 4)
            rock = ROCKS[next_rock_idx]
            next_rock_idx = (next_rock_idx % length(ROCKS)) + 1
        end

        # Apply jet
        jet = jets[next_jet_idx]
        next_jet_idx = (next_jet_idx % length(jets)) + 1
        new_pos = pos + CartesianIndex(0, jet == '>' ? 1 : -1)
        if !check_collision(cave, rock, new_pos)
            pos = new_pos
        end

        # Apply gravity
        pos -= CartesianIndex(1, 0)
        if check_collision(cave, rock, pos)
            add_rock!(cave, rock, pos)
            total_rocks += 1
            height = max(height, pos[1] + size(rock, 1))
            rock = nothing

            # Detect cycles
            if detect_cycles && height > 100
                h = hash(cave[height-100:height, 1:end])
                if haskey(seen, h)
                    last_height, last_rocks = seen[h]
                    period = total_rocks - last_rocks
                    if (num_rocks - total_rocks) % period == 0
                        remaining_cycles = (num_rocks - total_rocks) ÷ period
                        return height + (height - last_height) * remaining_cycles - 1
                    end
                else
                    seen[h] = (height, total_rocks)
                end
            end
        end
    end
    return height - 1
end

function part_a(data)
    return drop_rocks(data, 2022)
end

function part_b(data)
    return drop_rocks(data, 1000000000000, true)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day17

if abspath(PROGRAM_FILE) == @__FILE__
    Day17.main()
end
