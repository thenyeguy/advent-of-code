module Day22

using Combinatorics

#
# Data loading:
function load_data()
    function parse_line(l)
        on = l[1:2] == "on"
        is = eachmatch(r"-?\d+", l) |> collect .|> m->parse(Int,getfield(m,:match))
        return (on, (is[1]:is[2], is[3]:is[4], is[5]:is[6]))
    end
    return parse_line.(eachline(joinpath(@__DIR__, "data", "22.txt")))
end

#
# Common code:
grid_volume(g) = prod(length.(g))
grid_intersect(g1, g2) = intersect.(g1, g2)
grid_overlaps(g1,g2) = grid_volume(grid_intersect(g1, g2)) > 0

function grid_subtract(g1, g2)
    # Check for overlaps before subtracting
    grid_overlaps(g1, g2) || return [g1]

    # Compute the endpoints for both ranges.
    g1_min = minimum.(g1)
    g1_max = maximum.(g1)
    g2_min = minimum.(g2)
    g2_max = maximum.(g2)

    # Divide g1 into 27 possible subgrids, assuming that g2 is
    # entirely contained inside g1 (excluding g2 itself).
    r1s = (g1_min[1]:g2_min[1]-1, g2[1], g2_max[1]+1:g1_max[1])
    r2s = (g1_min[2]:g2_min[2]-1, g2[2], g2_max[2]+1:g1_max[2])
    r3s = (g1_min[3]:g2_min[3]-1, g2[3], g2_max[3]+1:g1_max[3])

    idx_to_grid(is) = (r1s[is[1]], r2s[is[2]], r3s[is[3]])

    indices = filter(is -> is != [2,2,2],
                     collect(multiset_permutations(1:3, [3,3,3], 3)))

    # Select valid grids: limit them to be inside g1, and of nonzero volume.
    subgrids = map(idx -> grid_intersect(idx_to_grid(idx), g1), indices)
    return filter(g -> grid_volume(g) > 0, subgrids)
end

function reboot(steps)
    ranges = []
    for (on, new_range) in steps
        new_ranges = []
        for old_range in ranges
            append!(new_ranges, grid_subtract(old_range, new_range))
        end
        on && push!(new_ranges, new_range)
        ranges = new_ranges
    end
    return mapreduce(grid_volume, +, ranges)
end

#
# Part A:
function part_a(data)
    # First 20 points are inside (-50,50), by inspection.
    return reboot(data[1:20])
end

#
# Part B:
function part_b(data)
    return reboot(data)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day22

if abspath(PROGRAM_FILE) == @__FILE__
    Day22.main()
end
