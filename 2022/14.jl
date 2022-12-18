module Day14

function parse_line(line)
    function parse_point(str)
        sp = split(str, ",")
        return [parse(Int, sp[1]), parse(Int, sp[2])]
    end
    return parse_point.(split(line, " -> "))
end

function load_data()
    return parse_line.(eachline(joinpath(@__DIR__, "14.txt")))
end

function get_bounds(data)
    xs = map(p->p[1], Iterators.flatten(data))
    ys = map(p->p[2], Iterators.flatten(data))
    return ((minimum(xs), minimum(ys)), (maximum(xs), maximum(ys)))
end

function draw_bounds(data)
    ((min_x, _), (max_x, max_y)) = get_bounds(data)
    height = max_y
    bounds = zeros(Bool, 1000, height+1)
    for line in data
        for i in 1:(length(line) - 1)
            p = line[i]
            p2 = line[i+1]
            dir = sign.(p2 - p)
            while p != p2
                bounds[CartesianIndex(p[1], p[2]+1)] = true
                p += dir
            end
            bounds[CartesianIndex(p[1], p[2]+1)] = true
        end
    end
    return bounds
end

function add_sand!(bounds)
    (width, height) = size(bounds)
    sand = [width÷2, 1]
    while sand[2] < height && !bounds[CartesianIndex(sand[1], sand[2])]
        if !bounds[CartesianIndex(sand[1], sand[2]+1)]
            sand +=  [0,1]
        elseif !bounds[CartesianIndex(sand[1]-1, sand[2]+1)]
            sand += [-1,1]
        elseif !bounds[CartesianIndex(sand[1]+1, sand[2]+1)]
            sand += [1,1]
        else
            bounds[CartesianIndex(sand[1], sand[2])] = true
            return true
        end
    end
    return false
end

function fill_with_sand!(bounds)
    sand = 0
    while add_sand!(bounds)
        sand += 1
    end
    return sand
end

function part_a(data)
    return data |> draw_bounds |> fill_with_sand!
end

function add_floor(bounds)
    width = size(bounds, 1)
    return hcat(bounds, zeros(Bool, width, 1), ones(Bool, width, 1))
end

function part_b(data)
    return data |> draw_bounds |> add_floor |> fill_with_sand!
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day14

if abspath(PROGRAM_FILE) == @__FILE__
    Day14.main()
end
