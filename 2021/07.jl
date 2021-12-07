module Day7

function load_data()
    return parse.(Int, split(read(joinpath(@__DIR__, "07.txt"), String), ","))
end

function find_minimum_fuel(compute_consumption, data)
    min_fuel = Inf
    for i in minimum(data):maximum(data)
        fuel = mapreduce(x -> compute_consumption(x, i), +, data)
        if fuel < min_fuel
            min_fuel = fuel
        end
    end
    return min_fuel
end

function part_a(data)
    return find_minimum_fuel((x,i) -> abs(x-i), data)
end

function part_b(data)
    function compute_consumption(x,i)
        dist = abs(x-i)
        return dist*(dist+1)÷2
    end
    return find_minimum_fuel(compute_consumption, data)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day7

if abspath(PROGRAM_FILE) == @__FILE__
    Day7.main()
end
