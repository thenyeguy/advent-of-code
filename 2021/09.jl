module Day9

#
# Data loading:
function load_data()
    lines = eachline(joinpath(@__DIR__, "09.txt"))
    return mapreduce(l -> parse.(Int, split(l, "")), hcat, lines)
end

#
# Part A:
function neighbors(data, idx)
    return filter(idx -> checkbounds(Bool, data, idx),
                  map(didx -> idx+CartesianIndex(didx),
                      [(-1,0), (1,0), (0,-1), (0,1)]))
end

function find_low_points(data)
    return filter(idx -> all(nidx -> data[idx] < data[nidx], neighbors(data, idx)),
                  CartesianIndices(data))
end

function part_a(data)
    return mapreduce(idx -> data[idx]+1, +, find_low_points(data))
end

#
# Part B:
function basin_size(data, low_point)
    frontier = [low_point]
    visited = Set()
    while !isempty(frontier)
        idx = pop!(frontier)
        if idx in visited || data[idx] == 9
            continue
        end
        push!(visited, idx)
        append!(frontier, neighbors(data, idx))
    end
    return length(visited)
end

function part_b(data)
    basins = map(p -> basin_size(data, p), find_low_points(data))
    return prod(sort(basins, rev=true)[1:3])
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day9

if abspath(PROGRAM_FILE) == @__FILE__
    Day9.main()
end
