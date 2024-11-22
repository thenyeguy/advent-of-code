module Day9

function load_data()
    function parse_line(line)
        dir, steps = split(line, " ")
        return (dir, parse(Int, steps))
    end
    return parse_line.(eachline(joinpath(@__DIR__, "data", "09.txt")))
end

function move(rope, dir_str)
    if     dir_str == "R" dir = [1, 0]
    elseif dir_str == "L" dir = [-1,0]
    elseif dir_str == "U" dir = [0, 1]
    elseif dir_str == "D" dir = [0,-1]
    end

    new_rope = [rope[1] + dir]
    for i in 2:length(rope)
        head = new_rope[i-1]
        tail = rope[i]
        dist = sum(abs.(head - tail))
        if dist == 2 && (head[1] == tail[1] || head[2] == tail[2])
            # Same row or col, move orthogonally
            tail = (head + tail) / 2
        elseif dist > 2
            # Off axis, move diagonally
            dir = sign.(head - tail)
            tail = tail + dir
        end
        push!(new_rope, tail)
    end
    return new_rope
end

function apply_moves(moves, knots)
    rope = []
    for _ in 1:knots
        push!(rope, [0,0])
    end

    visited = Set()
    for (dir, count) in moves
        for _ in 1:count
            rope = move(rope, dir)
            push!(visited, rope[end])
        end
    end
    return length(visited)
end

function part_a(data)
    return apply_moves(data, 2)
end

function part_b(data)
    return apply_moves(data, 10)
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
