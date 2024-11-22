module Day8

function to_matrix(lines)
    return parse.(Int, reduce(vcat, permutedims.(collect.(lines))))
end

function load_data()
    return to_matrix(eachline(joinpath(@__DIR__, "data", "08.txt")))
end

function find_heights(data)
    function _project_heights(data, dim)
        data = rotl90(data, dim)
        heights = -ones(Int, size(data))
        for i in 2:size(data,2)
            heights[i,:] = max.(heights[i-1,:], data[i-1,:])
        end
        return rotr90(heights, dim)
    end
    return mapreduce(d -> _project_heights(data, d), (l,r) -> min.(l,r), 0:3)
end

function part_a(data)
    return sum(data .> find_heights(data))
end

function scenic_score(data, idx)
    height = data[idx]
    function _traverse(dir)
        trees = 0
        cur = idx
        while true
            cur += dir
            checkbounds(Bool, data, cur) || break
            trees += 1
            data[cur] < height || break
        end
        return trees
    end
    return mapreduce(_traverse, *, CartesianIndex.([(1,0), (-1,0), (0,1), (0,-1)]))
end

function part_b(data)
    return mapreduce(idx -> scenic_score(data, idx), max, CartesianIndices(data))
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day8

if abspath(PROGRAM_FILE) == @__FILE__
    Day8.main()
end
