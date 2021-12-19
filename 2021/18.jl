module Day18

#
# Data loading:
function load_data()
    return eachline(joinpath(@__DIR__, "18.txt")) .|> Meta.parse .|> eval
end

#
# Common code:
struct SnailfishLeaf
    value::Int
    depth::Int
end

function flatten_snailfish(n, depth=0)
    if isa(n, Int)
        return SnailfishLeaf(n, depth)
    else
        return vcat(flatten_snailfish(n[1], depth+1),
                    flatten_snailfish(n[2], depth+1))
    end
end

function reduce_snailfish(leaves)
    function _reduce(leaves)
        for (i,leaf) in enumerate(leaves)
            if leaf.depth > 4
                left = leaves[1:i-1]
                if i > 1
                    left[end] = SnailfishLeaf(left[end].value + leaf.value,
                                              left[end].depth)
                end

                right = leaves[i+2:end]
                if i < length(leaves)-1
                    right[1] = SnailfishLeaf(right[1].value + leaves[i+1].value,
                                             right[1].depth)
                end

                return vcat(left, SnailfishLeaf(0, leaf.depth-1), right), false
            end
        end

        for (i,leaf) in enumerate(leaves)
            if leaf.value >= 10
                l = SnailfishLeaf(Int(floor(leaf.value/2)), leaf.depth+1)
                r = SnailfishLeaf(Int(ceil(leaf.value/2)), leaf.depth+1)
                return vcat(leaves[1:i-1], l, r, leaves[i+1:end]), false
            end
        end

        return leaves, true
    end

    while true
        leaves, done = _reduce(leaves)
        done && break
    end
    return leaves
end

function add_snailfish(l, r)
    pair = map(l -> SnailfishLeaf(l.value, l.depth+1), vcat(l, r))
    return reduce_snailfish(pair)
end

function expand_snailfish(leaves)
    function _expand(leaves, depth=0)
        if leaves[1].depth > depth
            left, leaves = _expand(leaves, depth+1)
        else
            left = leaves[1].value
            leaves = leaves[2:end]
        end

        if isempty(leaves)
            return left, []
        end

        if leaves[1].depth > depth
            right, leaves = _expand(leaves, depth+1)
        else
            right = leaves[1].value
            leaves = leaves[2:end]
        end
        return [left, right], leaves
    end

    pair, _ = _expand(leaves)
    return pair
end

function magnitude_snailfish(pair)
    if isa(pair, Int)
        return pair
    else
        return 3*magnitude_snailfish(pair[1]) + 2*magnitude_snailfish(pair[2])
    end
end

#
# Part A:
function part_a(data)
    total = foldl(add_snailfish, flatten_snailfish.(data))
    return total |> expand_snailfish |> magnitude_snailfish
end

#
# Part B:
function part_b(data)
    numbers = flatten_snailfish.(data)
    max_magnitude = 0
    for i in 1:length(data)
        for j in 1:length(data)
            i == j && continue
            s = add_snailfish(numbers[i], numbers[j])
            m = s |> expand_snailfish |> magnitude_snailfish
            max_magnitude = max(max_magnitude, m)
        end
    end
    return max_magnitude
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day18

if abspath(PROGRAM_FILE) == @__FILE__
    Day18.main()
end
