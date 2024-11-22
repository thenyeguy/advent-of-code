module Day23

function to_matrix(lines)
    return reduce(vcat, permutedims.(collect.(lines)))
end

function load_data()
    return to_matrix(eachline(joinpath(@__DIR__, "data", "23.txt")))
end

function pad(data, padding)
    height = size(data, 1)
    padded = hcat(fill('.', height, padding), data, fill('.', height, padding))
    width = size(padded, 2)
    return vcat(fill('.', padding, width), padded, fill('.', padding, width))
end

function neighborhood(data, idx)
    x = idx[1]
    y = idx[2]
    return data[x-1:x+1, y-1:y+1]
end

function count_elves(data)
    ELF = "#"[1] # fix syntax highlighting...
    return count(==(ELF), data)
end

function scan_dir(neighborhood, dir)
    if dir == 'N'
        return count_elves(neighborhood[1, 1:3])
    elseif dir == 'S'
        return count_elves(neighborhood[3, 1:3])
    elseif dir == 'W'
        return count_elves(neighborhood[1:3, 1])
    elseif dir == 'E'
        return count_elves(neighborhood[1:3, 3])
    end
end

function moves_to(proposals, idx)
    north = idx + CartesianIndex(-1,0)
    south = idx + CartesianIndex(1,0)
    west = idx + CartesianIndex(0,-1)
    east = idx + CartesianIndex(0,1)

    moves = []
    proposals[north] == 'S' && push!(moves, north)
    proposals[south] == 'N' && push!(moves, south)
    proposals[west] == 'E' && push!(moves, west)
    proposals[east] == 'W' && push!(moves, east)
    return moves
end

function simulate(data, rounds=10)
    ELF = "#"[1] # fix syntax highlighting...

    dirs = ['N', 'S', 'W', 'E']
    for round in 1:rounds
        data = pad(trim(data), 2)
        proposals = fill(' ', size(data))
        for idx in CartesianIndices(data)
            if data[idx] == ELF
                n = neighborhood(data, idx)
                if count_elves(n) == 1
                    continue
                end

                for dir in dirs
                    if scan_dir(n, dir) == 0
                        proposals[idx] = dir
                        break
                    end
                end
            end
        end

        total_moves = 0
        for idx in CartesianIndices((2:size(data,1)-1, 2:size(data,2)-1))
            moves = moves_to(proposals, idx)
            if length(moves) == 1
                data[idx] = ELF
                data[moves[1]] = '.'
                total_moves += 1
            end
        end
        if total_moves == 0
            return round
        end
        circshift!(dirs, -1)
    end
    return data
end

function trim(data)
    rows = 1:size(data,1)
    cols = 1:size(data,2)
    while count_elves(data[rows.start, 1:end]) == 0
        rows = rows.start+1:rows.stop
    end
    while count_elves(data[rows.stop, 1:end]) == 0
        rows = rows.start:rows.stop-1
    end
    while count_elves(data[1:end, cols.start]) == 0
        cols = cols.start+1:cols.stop
    end
    while count_elves(data[1:end, cols.stop]) == 0
        cols = cols.start:cols.stop-1
    end
    return data[rows, cols]
end

function part_a(data)
    result = data |> simulate |> trim
    return count(==('.'), result)
end

function part_b(data)
    return simulate(data, 1000000)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day23

if abspath(PROGRAM_FILE) == @__FILE__
    Day23.main()
end
