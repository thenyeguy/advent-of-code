module Day5

struct Move
    from::Int
    to::Int
    crates::Int
end

struct Plan
    stacks::Vector{Vector{Char}}
    moves::Vector{Move}
end

function load_data()
    function parse_move(line)
        m = match(r"move (\d+) from (\d) to (\d)", line)
        return Move(
            parse(Int, m.captures[2]),
            parse(Int, m.captures[3]),
            parse(Int, m.captures[1])
        )
    end

    function get_stack(stacks, i)
        stack = filter(c -> c != ' ', getindex.(stacks, i*4-2))
        return reverse(stack)
    end

    raw_stacks, raw_moves = split(read(joinpath(@__DIR__, "05.txt"), String), "\n\n")
    raw_stacks = split(raw_stacks, "\n")[1:end-1]
    stacks = map(i -> get_stack(raw_stacks, i), 1:9)
    moves = parse_move.(split(raw_moves, "\n"))
    return Plan(stacks, moves)
end

function run_plan_9000(plan)
    stacks = deepcopy(plan.stacks)
    for move in plan.moves
        for i in 1:move.crates
            push!(stacks[move.to], pop!(stacks[move.from]))
        end
    end
    return stacks
end

function part_a(data)
    run_plan_9000(data) .|> last |> String
end

function run_plan_9001(plan)
    stacks = deepcopy(plan.stacks)
    for move in plan.moves
        crates = range(length(stacks[move.from])-move.crates+1, length=move.crates)
        stacks[move.to] = vcat(stacks[move.to], stacks[move.from][crates])
        deleteat!(stacks[move.from], crates)
    end
    return stacks
end

function part_b(data)
    run_plan_9001(data) .|> last |> String
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day5

if abspath(PROGRAM_FILE) == @__FILE__
    Day5.main()
end
