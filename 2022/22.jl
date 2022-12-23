module Day22

struct Board
    map::Matrix{Char}
    directions::Vector{Tuple{String, Int}}
end

function to_matrix(map)
    rows = split(map, "\n")
    width = maximum(length.(rows))
    height = length(rows)
    m = fill(' ', width, height)
    for i in 1:height
        for j in 1:length(rows[i])
            m[j,i] = rows[i][j]
        end
    end
    return m
end

function parse_directions(line)
    return map(m -> (m[1], parse(Int, m[2])), eachmatch(r"(\w)(\d+)", "R" * line))
end

function load_data()
    map, directions = split(read(joinpath(@__DIR__, "22.txt"), String), "\n\n")
    return Board(to_matrix(map), parse_directions(directions))
end

function compute_password(board, wrap_fn)
    pos = findfirst(==('.'), board.map)
    facing = 3
    for (turn, steps) in board.directions
        if turn == "R"
            facing = mod(facing + 1, 4)
        else
            facing = mod(facing - 1, 4)
        end

        for _ in 1:steps
            step = [
                CartesianIndex(1,0),
                CartesianIndex(0,1),
                CartesianIndex(-1,0),
                CartesianIndex(0,-1)
            ][facing + 1]
            next = pos + step

            # Wrap around
            if !checkbounds(Bool, board.map, next) || board.map[next] == ' '
                next, next_facing = wrap_fn(board, facing, pos)
                if board.map[next] == '.'
                    facing = next_facing
                end
            end

            # Try to step into the next space
            if board.map[next] == '.'
                pos = next
            elseif board.map[next] == '#'
                break
            else
                throw(InvalidStateException)
            end
        end
    end
    return 1000 * pos[2] + 4 * pos[1] + facing
end

function flat_wrap(board, facing, pos)
    if facing == 0
        c = findfirst(!=(' '), board.map[1:end, pos[2]])
        return CartesianIndex(c, pos[2]), facing
    elseif facing == 1
        c = findfirst(!=(' '), board.map[pos[1], 1:end])
        return CartesianIndex(pos[1], c), facing
    elseif facing == 2
        c = findfirst(!=(' '), board.map[end:-1:1, pos[2]])
        return CartesianIndex(size(board.map,1)-c+1, pos[2]), facing
    elseif facing == 3
        c = findfirst(!=(' '), board.map[pos[1], end:-1:1])
        return CartesianIndex(pos[1], size(board.map,2)-c+1), facing
    end
end

function part_a(data)
    return compute_password(data, flat_wrap)
end

function cube_wrap(board, facing, pos)
    # Faces (hard-coded to test input):
    #   1 2    x -> y
    #   3           v
    # 5 4
    # 6
    if pos[2] == 1 && pos[1] <= 100 && facing == 3
        # Top of 1 -> left of 6
        return CartesianIndex(1, pos[1]+100), 0
    elseif pos[1] == 1 && pos[2] > 150 && facing == 2
        # Left of 6 -> top of 1
        return CartesianIndex(pos[2]-100, 1), 1
    elseif pos[1] == 51 && pos[2] <= 50 && facing == 2
        # Left of 1 -> left of 5
        return CartesianIndex(1, 150-pos[2]+1), 0
    elseif pos[1] == 1 && 100 < pos[2] && pos[2] <= 150 && facing == 2
        # Left of 5 -> left of 1
        return CartesianIndex(51, 150-pos[2]+1), 0
    elseif pos[2] == 1 && pos[1] > 100 && facing == 3
        # Top of 2 -> bottom of 6
        return CartesianIndex(pos[1]-100, 200), 3
    elseif pos[2] == 200 && facing == 1
        # Bottom of 6 -> top of 2
        return CartesianIndex(pos[1]+100, 1), 1
    elseif pos[1] == 150 && facing == 0
        # Right of 2 -> right of 4
        return CartesianIndex(100, 150-pos[2]+1), 2
    elseif pos[1] == 100 && pos[2] > 100 && facing == 0
        # Right of 4 -> right of 2
        return CartesianIndex(150, 150-pos[2]+1), 2
    elseif pos[2] == 50 && facing == 1
        # Bottom of 2 -> right of 3
        return CartesianIndex(100, pos[1]-50), 2
    elseif pos[1] == 100 && 50 < pos[2] && pos[2] <= 100 && facing == 0
        # Right of 3 -> bottom of 2
        return CartesianIndex(pos[2]+50, 50), 3
    elseif pos[1] == 51 && 50 < pos[2] && pos[2] <= 100 && facing == 2
        # Left of 3 -> top of 5
        return CartesianIndex(pos[2]-50, 101), 1
    elseif pos[2] == 101 && facing == 3
        # Top of 5 -> left of 3
        return CartesianIndex(51, pos[1]+50), 0
    elseif pos[2] == 150 && facing == 1
        # Bottom of 4 -> right of 6
        return CartesianIndex(50, pos[1]+100), 2
    elseif pos[1] == 50 && pos[2] > 150 && facing == 0
        # Right of 6 -> bottom of 4
        return CartesianIndex(pos[2]-100, 150), 3
    else
        throw(InvalidStateException)
    end
end

function part_b(data)
    return compute_password(data, cube_wrap)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day22

if abspath(PROGRAM_FILE) == @__FILE__
    Day22.main()
end
