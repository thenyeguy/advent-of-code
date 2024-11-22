module Day4

function parse_pair(p)
    function parse_range(r)
        s, e = parse.(Int, split(r, "-"))
        return range(s, e)
    end
    return parse_range.(split(p, ","))
end

function load_data()
    return parse_pair.(eachline(joinpath(@__DIR__, "data", "04.txt")))
end

function pair_is_subset((a,b))
    return (a.start >= b.start && a.stop <= b.stop) ||
           (b.start >= a.start && b.stop <= a.stop)
end

function part_a(data)
    data .|> pair_is_subset |> sum
end

function pair_overlaps((a,b))
    return !(a.stop < b.start || a.start > b.stop)
end

function part_b(data)
    data .|> pair_overlaps |> sum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day4

if abspath(PROGRAM_FILE) == @__FILE__
    Day4.main()
end
