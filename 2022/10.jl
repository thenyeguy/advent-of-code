module Day10

function load_data()
    return collect(eachline(joinpath(@__DIR__, "10.txt")))
end

function execute_instruction(instruction, value)
    if instruction == "noop"
        return [value]
    else
        arg = parse(Int, split(instruction, " ")[2])
        return [value, value + arg]
    end
end

function execute(program)
    values = [1]
    for instruction in program
        append!(values, execute_instruction(instruction, values[end]))
    end
    return values
end

function signal_strength(cycles)
    strength = 0
    for i in 20:40:220
        value = cycles[i]
        strength += i * value
    end
    return strength
end

function part_a(data)
    return data |> execute |> signal_strength
end

function draw(cycles)
    rows = []
    for i in 0:40:219
        row = []
        for j in 1:40
            value = cycles[i+j]
            visible = abs(value - j + 1) < 2
            push!(row, visible ? "#" : ".")
        end
        push!(rows, row)
    end
    return rows
end

function part_b(data)
    result = data |> execute |> draw
    return "\n" * join(prod.(result), "\n")
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day10

if abspath(PROGRAM_FILE) == @__FILE__
    Day10.main()
end
