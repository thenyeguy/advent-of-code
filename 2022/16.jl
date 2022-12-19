module Day16

using Combinatorics
using DataStructures

struct Valve
    name::String
    flow::Int
    tunnels::Array{String}
end

function parse_valve(line)
    name = match(r"[A-Z]{2}", line).match
    flow = parse(Int, match(r"\d+", line).match)
    tunnels = getfield.(eachmatch(r"[A-Z]{2}", split(line, ";")[end]), :match)
    return Valve(name, flow, tunnels)
end

function load_data()
    return parse_valve.(eachline(joinpath(@__DIR__, "16.txt")))
end

struct Layout
    targets::Array{String}
    flows::Dict{String, Int}
    costs::Dict{String, Dict{String, Int}}
end

function compute_costs(edges, from, to)
    costs = Dict()
    seen = Set()
    frontier = PriorityQueue(from=>0)
    while !isempty(frontier)
        (src, cost) = peek(frontier)
        delete!(frontier, src)
        src in seen && continue
        push!(seen, src)
        if src in to
            costs[src] = cost
        end
        for dest in edges[src]
            haskey(frontier, dest) && continue
            enqueue!(frontier, dest=>(cost+1))
        end
    end
    return costs
end

function compute_layout(valves)
    targets = collect(map(v -> v.name, filter(v -> v.flow > 0, valves)))
    flows = Dict(map(v -> (v.name, v.flow), filter(v -> v.flow > 0, valves)))
    edges = Dict(map(v -> (v.name, v.tunnels), valves))
    costs = Dict(map(v -> (v, compute_costs(edges, v, targets)), targets))
    costs["AA"] = compute_costs(edges, "AA", targets)
    return Layout(targets, flows, costs)
end

struct State
    open::Array{String}
    closed::Set{String}
    remaining_time::Int
    pressure::Int
end

# Computes how much pressure is released by every possible order of valve opening.
function compute_pressures(layout, time=30)
    pressures = Dict()
    states = [State(["AA"], Set(layout.targets), time, 0)]
    while !isempty(states)
        state = pop!(states)
        for valve in state.closed
            pos = state.open[end]
            remaining_time = state.remaining_time - layout.costs[pos][valve] - 1
            remaining_time < 1 && continue

            open = vcat(state.open, valve)
            haskey(pressures, open) && continue

            closed = setdiff(state.closed, Set([valve]))
            pressure = state.pressure + remaining_time * layout.flows[valve]
            pressures[open] = pressure
            push!(states, State(open, closed, remaining_time, pressure))
        end
    end
    return pressures
end

function part_a(data)
    layout = compute_layout(data)
    pressures = compute_pressures(layout, 30)
    return maximum(values(pressures))
end

function part_b(data)
    layout = compute_layout(data)
    pressures = compute_pressures(layout, 26)

    # Convert pressures to be independent of valve order.
    best_pressures = Dict()
    for (order, pressure) in pressures
        key = Set(order[2:end])
        if haskey(best_pressures, key)
            best_pressures[key] = max(best_pressures[key], pressure)
        else
            best_pressures[key] = pressure
        end
    end

    # Partition the valves to open between me and the elephant.
    # We must include a third unvisited parition to account for unreachable valves.
    max_pressure = 0
    for num_partitions in [2,3]
        for ps in partitions(layout.targets, num_partitions)
            me = Set(ps[1])
            elephant = Set(ps[2])
            haskey(best_pressures, me) && haskey(best_pressures, elephant) || continue
            pressure = best_pressures[me] + best_pressures[elephant]
            max_pressure = max(max_pressure, pressure)
        end
    end
    return max_pressure
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day16

if abspath(PROGRAM_FILE) == @__FILE__
    Day16.main()
end
