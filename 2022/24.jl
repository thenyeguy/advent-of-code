module Day24

using DataStructures

function to_matrix(lines)
    return reduce(vcat, permutedims.(collect.(lines)))
end

function load_data()
    return to_matrix(eachline(joinpath(@__DIR__, "data", "24.txt")))
end

struct Blizzards
    width::Int
    height::Int
    lefts::Vector{CartesianIndex}
    rights::Vector{CartesianIndex}
    ups::Vector{CartesianIndex}
    downs::Vector{CartesianIndex}
end

function load_blizzards(data)
    lefts = []
    rights = []
    ups = []
    downs = []
    for idx in CartesianIndices(data)
        data[idx] == '<' && push!(lefts, idx)
        data[idx] == '>' && push!(rights, idx)
        data[idx] == '^' && push!(ups, idx)
        data[idx] == 'v' && push!(downs, idx)
    end
    return Blizzards(size(data,2), size(data,1), lefts, rights, ups, downs)
end

function has_blizzard(blizzards, idx)
    return idx in blizzards.lefts || idx in blizzards.rights ||
           idx in blizzards.ups || idx in blizzards.downs
end

function tick_blizzards(blizzards)
    lefts = []
    rights = []
    ups = []
    downs = []
    for idx in blizzards.lefts
        push!(lefts, CartesianIndex(idx[1], (idx[2] == 2 ? blizzards.width : idx[2])-1))
    end
    for idx in blizzards.rights
        push!(rights, CartesianIndex(idx[1], (idx[2] == blizzards.width-1 ? 1 : idx[2])+1))
    end
    for idx in blizzards.ups
        push!(ups, CartesianIndex((idx[1] == 2 ? blizzards.height : idx[1])-1, idx[2]))
    end
    for idx in blizzards.downs
        push!(downs, CartesianIndex((idx[1] == blizzards.height-1 ? 1 : idx[1])+1, idx[2]))
    end
    return Blizzards(blizzards.width, blizzards.height, lefts, rights, ups, downs)
end

function reachable(pos, width, height)
    function is_valid(p)
        if p == CartesianIndex(1,2)
            return true
        elseif p == CartesianIndex(height, width-1)
            return true
        else
            return p[1] in 2:height-1 && p[2] in 2:width-1
        end
    end

    return filter(is_valid, [
        pos,
        pos-CartesianIndex(1,0),
        pos+CartesianIndex(1,0),
        pos-CartesianIndex(0,1),
        pos+CartesianIndex(0,1),
    ])
end

function find_path(data, time=0, reverse=false)
    height, width = size(data)
    start = CartesianIndex(1,2)
    dest = CartesianIndex(height,width-1)

    blizzards = [tick_blizzards(load_blizzards(data))]

    seen = Set()
    frontier = Queue{Tuple{CartesianIndex,Int}}()
    if reverse
        enqueue!(frontier, (dest, time))
    else
        enqueue!(frontier, (start, time))
    end

    while !isempty(frontier)
        pos, time = dequeue!(frontier)
        (pos,time) in seen && continue
        push!(seen, (pos,time))

        if reverse
            pos == start && return time
        else
            pos == dest && return time
        end

        time += 1
        while length(blizzards) < time
            push!(blizzards, tick_blizzards(blizzards[end]))
        end
        for next in reachable(pos, width, height)
            if !has_blizzard(blizzards[time], next)
                enqueue!(frontier, (next,time))
            end
        end
    end
end

function part_a(data)
    return find_path(data)
end

function part_b(data)
    t1 = find_path(data, 0, false)
    t2 = find_path(data, t1, true)
    t3 = find_path(data, t2, false)
    return t3
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day24

if abspath(PROGRAM_FILE) == @__FILE__
    Day24.main()
end
