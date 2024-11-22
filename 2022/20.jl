module Day20

function load_data()
    return parse.(Int, eachline(joinpath(@__DIR__, "data", "20.txt")))
end

function mix(data, rounds=1)
    pairs = collect(enumerate(data))
    for _ in 1:rounds
        for i in eachindex(data)
            idx = findfirst(p -> p[1] == i, pairs)
            new_idx = mod(idx + pairs[idx][2] - 2, length(data) - 1) + 2
            if new_idx < idx
                pairs = vcat(pairs[1:new_idx-1], pairs[idx], pairs[new_idx:idx-1], pairs[idx+1:end])
            else
                pairs = vcat(pairs[1:idx-1], pairs[idx+1:new_idx], pairs[idx], pairs[new_idx+1:end])
            end
        end
    end
    return map(p->p[2], pairs)
end

function get_grove_coordinates(mixed)
    function get_idx(idx)
        idx = mod(idx - 1, length(mixed)) + 1
        return mixed[idx]
    end
    offset = findfirst(==(0), mixed)
    return map(i -> get_idx(i), offset .+ [1000, 2000, 3000])
end

function part_a(data)
    return data |> mix |> get_grove_coordinates |> sum
end

function part_b(data)
    return mix(data * 811589153, 10) |> get_grove_coordinates |> sum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day20

if abspath(PROGRAM_FILE) == @__FILE__
    Day20.main()
end
