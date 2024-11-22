module Day3

function load_data()
    return collect(eachline(joinpath(@__DIR__, "data", "03.txt")))
end

function priority(item)
    if item <= 'Z'
        return item - 'A' + 27
    else
        return item - 'a' + 1
    end
end

function find_duplicate(rucksack)
    first = rucksack[1:length(rucksack)÷2]
    second = rucksack[length(rucksack)÷2+1:end]
    return collect(intersect(first, second))[1]
end

function part_a(data)
    return data .|> find_duplicate .|> priority |> sum
end

function find_shared_item(rucksacks)
    return collect(intersect(rucksacks[1], rucksacks[2], rucksacks[3]))[1]
end

function part_b(data)
    return Iterators.partition(data, 3) .|> find_shared_item .|> priority |> sum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day3

if abspath(PROGRAM_FILE) == @__FILE__
    Day3.main()
end
