module Day5

struct Line
    x1::Int
    y1::Int
    x2::Int
    y2::Int
end

function parse_line(s)
    ms = match(r"(\d+),(\d+) -> (\d+),(\d+)", s)
    ns = parse.(Int, ms.captures)
    return Line(ns[1], ns[2], ns[3],ns[4])
end

function load_data()
    return parse_line.(eachline(joinpath(@__DIR__, "data", "05.txt")))
end

function count_vents(data, include_diagonals)
    map = zeros(Int, 1000, 1000)
    for line in data
        if line.x1 == line.x2
            ys = range(line.y1, stop=line.y2, step=line.y2 > line.y1 ? 1 : -1)
            xs = line.x1 * ones(Int, length(ys))
        elseif line.y1 == line.y2
            xs = range(line.x1, stop=line.x2, step=line.x2 > line.x1 ? 1 : -1)
            ys = line.y1 * ones(Int, length(xs))
        elseif include_diagonals
            xs = range(line.x1, stop=line.x2, step=line.x2 > line.x1 ? 1 : -1)
            ys = range(line.y1, stop=line.y2, step=line.y2 > line.y1 ? 1 : -1)
        else
            continue
        end
        for (x, y) in zip(xs, ys)
            map[x, y] += 1
        end
    end
    return length(filter(x -> x >= 2, map))
end

function part_a(data)
    return count_vents(data, false)
end

function part_b(data)
    return count_vents(data, true)
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
