module Day21

struct NumberMonkey
    name::String
    number::Int
end

struct MathMonkey
    name::String
    left::String
    right::String
    operation::Function
end

function parse_monkey(line)
    sp = split(line, ": ")
    name = sp[1]
    if isnothing(match(r"\d+", sp[2]))
        m = match(r"(\w+) (.+) (\w+)", sp[2])
        if m[2] == "+" op = +
        elseif m[2] == "-" op = -
        elseif m[2] == "*" op = *
        elseif m[2] == "/" op = ÷
        end
        return MathMonkey(name, m[1], m[3], op)
    else
        return NumberMonkey(name, parse(Int, sp[2]))
    end
    return name
end

function as_dict(monkeys)
end

function load_data()
    monkeys = parse_monkey.(eachline(joinpath(@__DIR__, "21.txt")))
    return Dict(map(m -> (m.name, m), monkeys))
end

function get_tree_value(monkeys, root="root")
    function recurse(name)
        monkey = monkeys[name]
        if monkey isa NumberMonkey
            return monkey.number
        else
            return monkey.operation(recurse(monkey.left), recurse(monkey.right))
        end
    end
    return recurse(root)
end

function part_a(data)
    return get_tree_value(data)
end

function get_parents(monkeys)
    parents = Dict()
    function recurse(name)
        monkey = monkeys[name]
        if monkey isa MathMonkey
            parents[monkey.left] = name
            parents[monkey.right] = name
            recurse(monkey.left)
            recurse(monkey.right)
        end
    end
    recurse("root")
    return parents
end

function get_human_value(monkeys)
    function has_human(name)
        monkey = monkeys[name]
        if monkey.name == "humn"
            return true
        elseif monkey isa NumberMonkey
            return false
        else
            return has_human(monkey.left) || has_human(monkey.right)
        end
    end

    human_value = nothing
    function recurse(name, target)
        if name == "humn"
            human_value = target
            return
        end

        monkey = monkeys[name]
        @assert monkey isa MathMonkey
        if has_human(monkey.left)
            value = get_tree_value(monkeys, monkey.right)
            if monkey.operation == +
                sub_target = target - value
            elseif monkey.operation == -
                sub_target = target + value
            elseif monkey.operation == *
                sub_target = target ÷ value
            else
                sub_target = target * value
            end
            recurse(monkey.left, sub_target)
        else
            value = get_tree_value(monkeys, monkey.left)
            if monkey.operation == +
                sub_target = target - value
            elseif monkey.operation == -
                sub_target = value - target
            elseif monkey.operation == *
                sub_target = target ÷ value
            else
                sub_target = value ÷ target
            end
            recurse(monkey.right, sub_target)
        end
    end

    root = monkeys["root"]
    if has_human(root.left)
        recurse(root.left, get_tree_value(monkeys, root.right))
    else
        recurse(root.right, get_tree_value(monkeys, root.left))
    end
    return human_value
end

function part_b(data)
    return get_human_value(data)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day21

if abspath(PROGRAM_FILE) == @__FILE__
    Day21.main()
end
