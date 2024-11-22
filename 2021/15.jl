module Day15

using DataStructures

#
# Data loading:
function load_data()
    lines = eachline(joinpath(@__DIR__, "data", "15.txt"))
    return mapreduce(l -> parse.(Int, split(l, "")), hcat, lines)
end

#
# Common code:
function neighbors(data, idx)
    return filter(idx -> checkbounds(Bool, data, idx),
                  map(didx -> idx+CartesianIndex(didx),
                      [(-1,0), (1,0), (0,-1), (0,1)]))
end

function find_path(data)
    start = CartesianIndex(1,1)
    dest = CartesianIndex(size(data))

    frontier = PriorityQueue(CartesianIndices(data).=>Inf)
    frontier[start] = 0
    while !isempty(frontier)
        (idx, cost) = peek(frontier)
        if idx == dest
            return Int(cost)
        end

        delete!(frontier, idx)
        for neighbor in neighbors(data, idx)
            n_cost = cost + data[neighbor]
            if neighbor in keys(frontier) && n_cost < frontier[neighbor]
                frontier[neighbor] = n_cost
            end
        end
    end
end

#
# Part A:
function part_a(data)
    return find_path(data)
end

#
# Part B:
function expand(data)
    update_risk(tile) = mod.(tile, 9) .+ 1

    # Expand right
    expanded = data
    tile = data
    for i in 2:5
        tile = update_risk(tile)
        expanded = hcat(expanded, tile)
    end

    # Expand down
    tile = expanded
    for i in 2:5
        tile = update_risk(tile)
        expanded = vcat(expanded, tile)
    end

    expanded
end

function part_b(data)
    return data |> expand |> find_path
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day15

if abspath(PROGRAM_FILE) == @__FILE__
    Day15.main()
end
