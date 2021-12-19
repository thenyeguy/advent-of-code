module Day19

using LinearAlgebra

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
translate(p, dp) = p .+ dp
flip(p) = (-p[1], -p[2], p[3])
roll(p) = (p[1], -p[3], p[2])
rotate(p) = (p[2], p[3], p[1])

const Point = Tuple{Int, Int, Int}
const Fingerprint = Set{Float64}
struct Scanner
    points::Vector{Point}
    fingerprints::Vector{Fingerprint}
end

function compute_fingerprints(points)::Vector{Fingerprint}
    point_distance(p) = map(p2 -> norm(p2 .- p), points)
    return points .|> point_distance .|> Set
end

Scanner(points) = Scanner(points, compute_fingerprints(points))

function points_to_matrix(points)
    axis(i) = getindex.(points, i)
    return hcat(axis(1), axis(2), axis(3), ones(length(points)))
end

function matrix_to_points(mat)
    r(n) = Int(round(n))
    return tuple.(r.(mat[:,1]), r.(mat[:,2]), r.(mat[:,3]))
end

function align_scanners(s1::Scanner, s2::Scanner)
    points1 = []
    points2 = []
    for (p1,f1) in zip(s1.points, s1.fingerprints)
        for (p2,f2) in zip(s2.points, s2.fingerprints)
            if length(intersect(f1,f2)) >= 12
                push!(points1, p1)
                push!(points2, p2)
                length(points1) == 12 && @goto aligned
            end
        end
    end
    return nothing
    @label aligned

    transform = points_to_matrix(points2) \ points_to_matrix(points1)
    transformed = matrix_to_points(points_to_matrix(s2.points) * transform)
    merged = vcat(s1.points, filter(p -> !(p in s1.points), transformed))
    offset = tuple.(Int.(round.(transform[4,1:3]))...)
    return Scanner(merged), offset
end

function align_all_scanners(points)
    positions = [(0,0,0)]
    scanners = Scanner.(points)
    while length(scanners) > 1
        for i in 2:length(scanners)
            result = align_scanners(scanners[1], scanners[i])
            if !isnothing(result)
                push!(positions, result[2])
                scanners[1] = result[1]
                deleteat!(scanners, i)
                break
            end
        end
    end
    return scanners[1], positions
end

#
# Part A:
function part_a(data)
    aligned, _ = align_all_scanners(data)
    return length(aligned.points)
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
    _, positions = align_all_scanners(data)
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
