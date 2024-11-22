module Day12

using DataStructures

function to_matrix(lines)
    return reduce(vcat, permutedims.(collect.(lines)))
end

function load_data()
    return to_matrix(eachline(joinpath(@__DIR__, "data", "12.txt")))
end

function neighbors(data, idx)
    return filter(idx -> checkbounds(Bool, data, idx),
                  map(didx -> idx+CartesianIndex(didx),
                      [(-1,0), (1,0), (0,-1), (0,1)]))
end

function can_step(data, from, to)
    from_ = data[from] == 'S' ? 'a' : data[from]
    to_ = data[to] == 'E' ? 'z' : data[to]
    return (to_ - from_) <= 1
end

function find_path(data, starts)
    frontier = PriorityQueue()
    for start in starts
        frontier[start] = 0
    end

    visited = Set()
    while true
        (idx, steps) = peek(frontier)
        if data[idx] == 'E'
            return steps
        end

        delete!(frontier, idx)
        push!(visited, idx)

        for neighbor in neighbors(data, idx)
            if neighbor in visited || haskey(frontier, neighbor)
                continue
            elseif can_step(data, idx, neighbor)
                frontier[neighbor] = steps + 1
            end
        end
    end
end

function part_a(data)
    return find_path(data, findall(c -> c == 'S', data))
end

function part_b(data)
    return find_path(data, findall(c -> c == 'a', data))
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day12

if abspath(PROGRAM_FILE) == @__FILE__
    Day12.main()
end
