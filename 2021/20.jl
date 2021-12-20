module Day20

#
# Data loading:
function load_data()
    open(joinpath(@__DIR__, "20.txt"), "r") do io
        algorithm = readline(io) |> collect .|> ==('#') .|> Int
        readline(io)
        image = hcat(collect.(readlines(io))...) .|> ==('#') .|> Int
        return algorithm, image
    end
end

#
# Common code:
function pad(image, layers)
    col = zeros(Int, size(image,1), layers)
    row = zeros(Int, layers, size(image,2)+2*layers)
    return vcat(row, hcat(col, image, col), row)
end

function bound(image, idx)
    return max(CartesianIndex(1,1), min(CartesianIndex(size(image)), idx))
end

function neighborhood(image, idx)
    min_idx = bound(image, idx - CartesianIndex(1,1))
    max_idx = bound(image, idx + CartesianIndex(1,1))
    return image[min_idx:max_idx]
end

function enhance(algorithm, image; iterations=2)
    # An algorithm may lead to (9 empties->lit), in which case the infinite
    # frontier alternates between lit and unlit. One layer of padding is
    # required for each iteration due to natural expansion. A second layer of
    # padding insulates us from the frontier being lit, as we can crop out
    # the corrupted layers at the end.
    image = pad(image, iterations*2)
    for i in 1:iterations
        enhanced = zeros(Int, size(image))
        for idx in CartesianIndices(enhanced)
            ns = neighborhood(image, idx)
            code = foldl((total,bit) -> 2*total+bit, ns)
            enhanced[idx] = algorithm[code+1]
        end
        image = enhanced
    end
    return image[iterations+1:end-iterations,iterations+1:end-iterations]
end

#
# Part A:
function part_a(data)
    return enhance(data...) |> sum
end

#
# Part B:
function part_b(data)
    return enhance(data..., iterations=50) |> sum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day20

if abspath(PROGRAM_FILE) == @__FILE__
    Day20.main()
end
