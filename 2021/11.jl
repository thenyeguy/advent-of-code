module Day11

#
# Data loading:
function load_data()
    lines = eachline(joinpath(@__DIR__, "11.txt"))
    return mapreduce(l -> parse.(Int, split(l, "")), hcat, lines)
end

#
# Common code:
function neighbors(data, idx)
    return filter(idx -> checkbounds(Bool, data, idx),
                  map(didx -> idx+CartesianIndex(didx),
                      [(-1,-1), (0,-1), (1,-1),
                       (-1, 0),         (1, 0),
                       (-1, 1), (0, 1), (1, 1)]))
end

function step!(energies)
    energies .+= 1
    while true
        flashed = false
        for idx in CartesianIndices(energies)
            if energies[idx] > 9
                energies[idx] = 0
                flashed = true
                for nidx in neighbors(energies, idx)
                    if energies[nidx] > 0
                        energies[nidx] += 1
                    end
                end
            end
        end
        flashed || break
    end
    return count(==(0), energies)
end

#
# Part A:
function part_a(data)
    energies = copy(data)
    flashed = 0
    for i in 1:100
        flashed += step!(energies)
    end
    return flashed
end

#
# Part B:
function part_b(data)
    energies = copy(data)
    steps = 1
    while step!(energies) < length(energies)
        steps += 1
    end
    return steps
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day11

if abspath(PROGRAM_FILE) == @__FILE__
    Day11.main()
end
