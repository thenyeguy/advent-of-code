module Day15

struct Sensor
    pos::Array{Int}
    beacon::Array{Int}
end

Sensor(x1,y1,x2,y2) = Sensor([x1,y1], [x2,y2])

function parse_sensor(line)
    matches = parse.(Int, getfield.(eachmatch(r"-?\d+", line), :match))
    Sensor(matches...)
end

function load_data()
    return parse_sensor.(eachline(joinpath(@__DIR__, "15.txt")))
end

function get_range(sensor, row)
    radius = sum(abs.(sensor.pos - sensor.beacon))
    dist = abs(sensor.pos[2] - row)
    if dist <= radius
        r = radius - dist
        return sensor.pos[1] .+ (-r:r)
    end
end

function merge_ranges(sensors, row)
    ranges = filter(e -> !isnothing(e), get_range.(sensors, row)) |> sort
    merged = [ranges[1]]
    for r in ranges[2:end]
        cur = merged[end]
        if r.start - 1 <= cur.stop
            if r.stop > cur.stop
                merged[end] = cur.start:r.stop
            end
        else
            push!(merged, r)
        end
    end
    return merged
end

function known_beacons(sensors, row)
    return Set(filter(b -> b[2] == row, getfield.(sensors, :beacon)))
end

function empty_positions(sensors, row)
    ranges = merge_ranges(sensors, row)
    beacons = known_beacons(sensors, row)
    return sum(length.(ranges)) - length(beacons)
end

function part_a(data)
    return empty_positions(data, 2000000)
end

function find_beacon(sensors, search_range=4000000)
    for row in 0:search_range
        ranges = merge_ranges(sensors, row)
        if length(ranges) > 1
            return (row, ranges[1].stop + 1)
        end
    end
end

function signal_strength((row, col))
    return 4000000 * col + row
end

function part_b(data)
    return data |> find_beacon |> signal_strength
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day15

if abspath(PROGRAM_FILE) == @__FILE__
    Day15.main()
end
