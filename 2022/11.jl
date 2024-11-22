module Day11

struct Monkey
    items::Array{Int}
    operation::Function
    modulus::Int
    true_dest::Int
    false_dest::Int
end

function parse_monkey(lines)
    lines = split(lines, "\n")
    items = parse.(Int, split(split(lines[2], ": ")[2], ", "))
    modulus = parse.(Int, split(lines[4], " ")[end])
    true_dest = parse.(Int, split(lines[5], " ")[end])
    false_dest = parse.(Int, split(lines[6], " ")[end])

    op_line = lines[3]
    if occursin("old * old", op_line)
        op = x -> x*x
    elseif occursin("*", op_line)
        arg = parse.(Int, split(lines[3], " ")[end])
        op = x -> x*arg
    elseif occursin("+", op_line)
        arg = parse.(Int, split(lines[3], " ")[end])
        op = x -> x+arg
    end

    return Monkey(items, op, modulus, true_dest, false_dest)
end

function load_data()
    return parse_monkey.(split(read(joinpath(@__DIR__, "data", "11.txt"), String), "\n\n"))
end

function throw_items!(monkies, relief, base)
    inspected = []
    for monkey in monkies
        push!(inspected, length(monkey.items))
        for item in monkey.items
            if relief == 1
                worry = monkey.operation(item) % base
            else
                worry = monkey.operation(item) ÷ relief
            end
            dst = worry % monkey.modulus == 0 ? monkey.true_dest : monkey.false_dest
            push!(monkies[dst + 1].items, worry)
        end
        deleteat!(monkey.items, 1:length(monkey.items))
    end
    return inspected
end

function run_rounds(monkies, relief, rounds)
    monkies = deepcopy(monkies)
    base = prod(getfield.(monkies, :modulus))
    inspected = zeros(size(monkies))
    for i in 1:rounds
        inspected += throw_items!(monkies, relief, base)
    end
    return inspected
end

function compute_monkey_business(inspected_items)
    return sort(inspected_items)[end-1:end] |> prod |> Int
end

function part_a(data)
    return run_rounds(data, 3, 20) |> compute_monkey_business
end

function part_b(data)
    return run_rounds(data, 1, 10000) |> compute_monkey_business
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
