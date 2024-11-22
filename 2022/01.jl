module Day1

function load_data()
    data = split(read(joinpath(@__DIR__, "data", "01.txt"), String), "\n\n")
    return map(e -> parse.(Int, split(e, "\n")), data)
end

function part_a(data)
    return data .|> sum |> maximum
end

function part_b(data)
    sorted = sort(sum.(data), rev=true)
    return sum(sorted[1:3])
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day1

if abspath(PROGRAM_FILE) == @__FILE__
    Day1.main()
end
