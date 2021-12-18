module Day17

#
# Data loading:
struct Area
    xs::LinRange
    ys::LinRange
end

function load_data()
    return Area(265:287,-103:-58)
end

#
# Common code:
struct Probe
    x::Int
    y::Int
    dx::Int
    dy::Int
end

function step(p::Probe)
    return Probe(p.x+p.dx, p.y+p.dy, max(p.dx-1,0), p.dy-1)
end

struct Hit y_max::Int end
struct Miss end

function fire_probe((dx,dy), area)
    probe = Probe(0, 0, dx, dy)
    y_max = 0
    while true
        probe = step(probe)
        y_max = max(y_max, probe.y)
        if probe.x > maximum(area.xs) || probe.y < minimum(area.ys)
            return Miss
        elseif probe.x in area.xs && probe.y in area.ys
            return Hit(y_max)
        end
    end
end

function find_hits(area)
    # Exact bounds for x velocities.
    # Minimum x is solving for when sum(1:dx) == x_min.
    min_dx = -0.5 + sqrt(0.25 + 2*minimum(area.xs)) |> ceil |> Int
    max_dx = maximum(area.xs)

    hits = []
    for dx in min_dx:max_dx
        # Just guess at a reasonable range of y velocities.
        min_y = minimum(area.ys)
        for dy in min_y:-min_y
            result = fire_probe((dx,dy), area)
            if isa(result, Hit)
                push!(hits, result)
            end
        end
    end
    return hits
end

#
# Part A:
function part_a(area)
    return mapreduce(hit->hit.y_max, max, find_hits(area))
end

#
# Part B:
function part_b(area)
    return find_hits(area) |> length
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day17

if abspath(PROGRAM_FILE) == @__FILE__
    Day17.main()
end
