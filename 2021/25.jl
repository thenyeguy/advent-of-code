module Day25

#
# Data loading:
function load_data()
    return split.(eachline(joinpath(@__DIR__, "25.txt")), "")
end

#
# Common code:
function move_east_herd(map)
    new_map = deepcopy(map)
    for i in 1:length(map)
        for j in 1:length(map[i])
            right = (j % length(map[i])) + 1
            if map[i][j] == ">" && map[i][right] == "."
                new_map[i][right] = ">"
                new_map[i][j] = "."
            end
        end
    end
    return new_map
end

function move_south_herd(map)
    new_map = deepcopy(map)
    for i in 1:length(map)
        for j in 1:length(map[i])
            down = (i % length(map)) + 1
            if map[i][j] == "v" && map[down][j] == "."
                new_map[down][j] = "v"
                new_map[i][j] = "."
            end
        end
    end
    return new_map
end

move_herds(map) = map |> move_east_herd |> move_south_herd

#
# Part A:
function part_a(data)
    last_map = []
    map = data
    steps = 0
    while last_map != map
        steps += 1
        last_map = map
        map = move_herds(map)
    end
    return steps
end

#
# Part B:
function part_b(data)
    return "Merry Christmas!"
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day25

if abspath(PROGRAM_FILE) == @__FILE__
    Day25.main()
end
