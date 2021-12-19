module Day19

#
# Data loading:
function load_data()
    function parse_points(str)
        parse_point(m) = parse.(Int, (m[1], m[2], m[3]))
        eachmatch(r"(.+),(.+),(.+)", str) .|> parse_point |> collect
    end

    file = read(joinpath(@__DIR__, "19.txt"), String)
    split(file, "\n\n") .|> parse_points
end

#
# Common code:
translate(scanner, dp) = map(p -> p .+ dp, scanner)
flip(scanner) = map(t -> (-t[1], -t[2], t[3]), scanner)
roll(scanner) = map(t -> (t[1], -t[3], t[2]), scanner)
rotate(scanner) = map(t -> (t[2], t[3], t[1]), scanner)

function all_orientations(scanner)
    Channel() do channel
        for i in 1:3
            scanner = rotate(scanner)
            for j in 1:4
                scanner = roll(scanner)
                put!(channel, scanner)
                put!(channel, flip(scanner))
            end
        end
    end
end

function align_scanners(s1, s2)
    points = Set(s1)
    for oriented in all_orientations(s2)
        for p1 in s1
            for p2 in oriented
                aligned = Set(translate(oriented, p1 .- p2))
                if length(intersect(points, aligned)) >= 12
                    return collect(union(points, aligned)), p2 .- p1
                end
            end
        end
    end
end

function find_alignment(scanners)
    positions = [(0,0,0)]
    scanners = deepcopy(scanners)
    while length(scanners) > 1
        for i in 2:length(scanners)
            result = align_scanners(scanners[1], scanners[i])
            if !isnothing(result)
                scanners[1] = result[1]
                deleteat!(scanners, i)
                push!(positions, result[2])
                break
            end
        end
    end
    return scanners[1], positions
end

#
# Part A:
function part_a(data)
    beacons, _ = find_alignment(data)
    return length(beacons)
end

#
# Part B:
function largest_distance(positions)
    manhattan_distance(p1, p2) = sum(abs.(p1 .- p2))
    max_distance = 0
    for p1 in positions
        for p2 in positions
            max_distance = max(max_distance, manhattan_distance(p1, p2))
        end
    end
    return max_distance
end

function part_b(data)
    _, positions = find_alignment(data)
    return largest_distance(positions)
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
