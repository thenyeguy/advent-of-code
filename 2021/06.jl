module Day6

function load_data()
    return parse.(Int, split(read(joinpath(@__DIR__, "06.txt"), String), ","))
end

function breed_lanternfish(data, generations)
    counts = map(i -> count(==(i), data), 0:8)
    for i in 1:generations
        counts = circshift(counts, -1)
        counts[7] += counts[9]
    end
    return sum(values(counts))
end

function part_a(data)
    return breed_lanternfish(data, 80)
end

function part_b(data)
    return breed_lanternfish(data, 256)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day6

if abspath(PROGRAM_FILE) == @__FILE__
    Day6.main()
end
