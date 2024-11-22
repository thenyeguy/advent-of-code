module Day19

using DataStructures

struct Blueprint
    ore_robot_cost::Int
    clay_robot_cost::Int
    obsidian_robot_cost::Tuple{Int, Int}
    geode_robot_cost::Tuple{Int, Int}
end

function parse_blueprint(line)
    ore = match(r"ore robot costs (\d+)", line)
    clay = match(r"clay robot costs (\d+)", line)
    obsidian = match(r"obsidian robot costs (\d+) ore and (\d+)", line)
    geode = match(r"geode robot costs (\d+) ore and (\d+)", line)
    return Blueprint(
        parse(Int, ore[1]),
        parse(Int, clay[1]),
        (parse(Int, obsidian[1]),parse(Int, obsidian[2])),
        (parse(Int, geode[1]),parse(Int, geode[2])),
    )
end

function load_data()
    return parse_blueprint.(eachline(joinpath(@__DIR__, "data", "19.txt")))
end

function max_ore_cost(blueprint)
    return max(blueprint.ore_robot_cost, blueprint.clay_robot_cost,
               blueprint.obsidian_robot_cost[1], blueprint.geode_robot_cost[1])
end

struct State
    time::Int
    ore_robots::Int
    clay_robots::Int
    obsidian_robots::Int
    geode_robots::Int
    ore::Int
    clay::Int
    obsidian::Int
    geodes::Int
end

State() = State(0, 1, 0, 0, 0, 0, 0, 0, 0)

function build_ore_robot(blueprint, state)
    state.ore_robots >= max_ore_cost(blueprint) && return

    ore_wait = (blueprint.ore_robot_cost - state.ore) / state.ore_robots
    wait_time = max(0, ceil(ore_wait)) + 1

    return State(
        state.time + wait_time,
        state.ore_robots + 1,
        state.clay_robots,
        state.obsidian_robots,
        state.geode_robots,
        state.ore + wait_time * state.ore_robots - blueprint.ore_robot_cost,
        state.clay + wait_time * state.clay_robots,
        state.obsidian + wait_time * state.obsidian_robots,
        state.geodes,
    )
end

function build_clay_robot(blueprint, state)
    state.clay_robots >= blueprint.obsidian_robot_cost[2] && return

    ore_wait = (blueprint.clay_robot_cost - state.ore) / state.ore_robots
    wait_time = max(0, ceil(ore_wait)) + 1

    return State(
        state.time + wait_time,
        state.ore_robots,
        state.clay_robots + 1,
        state.obsidian_robots,
        state.geode_robots,
        state.ore + wait_time * state.ore_robots - blueprint.clay_robot_cost,
        state.clay + wait_time * state.clay_robots,
        state.obsidian + wait_time * state.obsidian_robots,
        state.geodes,
    )
end

function build_obsidian_robot(blueprint, state)
    state.clay_robots == 0 && return
    state.obsidian_robots >= blueprint.geode_robot_cost[2] && return

    ore_wait = (blueprint.obsidian_robot_cost[1] - state.ore) / state.ore_robots
    clay_wait = (blueprint.obsidian_robot_cost[2] - state.clay) / state.clay_robots
    wait_time = max(0, ceil(ore_wait), ceil(clay_wait)) + 1

    return State(
        state.time + wait_time,
        state.ore_robots,
        state.clay_robots,
        state.obsidian_robots + 1,
        state.geode_robots,
        state.ore + wait_time * state.ore_robots - blueprint.obsidian_robot_cost[1],
        state.clay + wait_time * state.clay_robots - blueprint.obsidian_robot_cost[2],
        state.obsidian + wait_time * state.obsidian_robots,
        state.geodes,
    )
end

function build_geode_robot(blueprint, state, minutes)
    state.obsidian_robots == 0 && return

    ore_wait = (blueprint.geode_robot_cost[1] - state.ore) / state.ore_robots
    obsidian_wait = (blueprint.geode_robot_cost[2] - state.obsidian) / state.obsidian_robots
    wait_time = max(0, ceil(ore_wait), ceil(obsidian_wait)) + 1

    geodes = minutes - (state.time + wait_time)
    geodes > 0 || return

    return State(
        state.time + wait_time,
        state.ore_robots,
        state.clay_robots,
        state.obsidian_robots,
        state.geode_robots + 1,
        state.ore + wait_time * state.ore_robots - blueprint.geode_robot_cost[1],
        state.clay + wait_time * state.clay_robots,
        state.obsidian + wait_time * state.obsidian_robots - blueprint.geode_robot_cost[2],
        state.geodes + geodes,
    )
end


function run_blueprint(blueprint, minutes=24)
    max_geodes = 0
    pq = PriorityQueue(State()=>0)
    seen = Set()

    function new_robot!(new_state)
        isnothing(new_state) && return
        new_state in seen && return
        if new_state.time < minutes
            enqueue!(pq, new_state=>new_state.time)
            push!(seen, new_state)
        else
            max_geodes = max(max_geodes, new_state.geodes)
        end
    end

    last_time = 0
    while !isempty(pq)
        state = dequeue!(pq) 
        new_robot!(build_ore_robot(blueprint, state))
        new_robot!(build_clay_robot(blueprint, state))
        new_robot!(build_obsidian_robot(blueprint, state))
        new_robot!(build_geode_robot(blueprint, state, minutes))
    end
    return max_geodes
end

function part_a(data)
    geodes = run_blueprint.(data)
    return mapreduce(prod, +, enumerate(geodes))
end

function part_b(data)
    geodes = run_blueprint.(data[1:3], 32)
    return prod(geodes)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day19

if abspath(PROGRAM_FILE) == @__FILE__
    Day19.main()
end
