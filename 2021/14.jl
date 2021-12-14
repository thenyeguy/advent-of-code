module Day14

using DataStructures

#
# Data loading:
function load_data()
    open(joinpath(@__DIR__, "14.txt"), "r") do io
        template = readline(io)
        readline(io)
        pairs = split.(eachline(io), " -> ") |> Dict
        return (template, pairs)
    end
end

#
# Common code:
function polymerize(template, pairs, iterations=1)
    counts = counter(String)
    for i in 1:length(template)-1
        inc!(counts, template[i:i+1])
    end

    for i in 1:iterations
        new_counts = counter(String)
        for (key, count) in counts
            inc!(new_counts, key[1] * pairs[key], count)
            inc!(new_counts, pairs[key] * key[2], count)
        end
        counts = new_counts
    end
    return counts
end

function score(pair_counts)
    base_counts = counter(Char)
    for (key, count) in pair_counts
        inc!(base_counts, key[1], count)
        inc!(base_counts, key[2], count)
    end
    # Every base is double counted (in two pairs) except for first and last.
    counts = values(base_counts) .|> c -> ceil(c/2) .|> Int
    return maximum(counts) - minimum(counts)
end

#
# Part A:
function part_a((template, pairs))
    return polymerize(template, pairs, 10) |> score
end

#
# Part B:
function part_b((template, pairs))
    return polymerize(template, pairs, 40) |> score
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
