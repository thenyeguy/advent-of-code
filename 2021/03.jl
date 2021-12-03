module Day3

function load_data()
    return collect(eachline(joinpath(@__DIR__, "03.txt")))
end

function most_common_bit(data, i)
    count = mapreduce(n -> parse(Int, n[i]), +, data)
    return count >= length(data)/2 ? '1' : '0'
end

function least_common_bit(data, i)
    return most_common_bit(data, i) == '1' ? '0' : '1'
end

function part_a(data)
    bits = length(data[1])
    gamma = mapreduce(i -> most_common_bit(data, i), *, 1:bits)
    epsilon = mapreduce(i -> least_common_bit(data, i), *, 1:bits)
    return parse(Int, gamma, base=2) * parse(Int, epsilon, base=2)
end

function part_b(data)
    function filter_by_bit(criteria, data, bit)
        if length(data) == 1
            return data[1]
        end

        c = criteria(data, bit)
        filtered = filter(n -> n[bit] == c, data)
        return filter_by_bit(criteria, filtered, bit+1)
    end

    oxygen = filter_by_bit(most_common_bit, data, 1)
    co2 = filter_by_bit(least_common_bit, data, 1)
    return parse(Int, oxygen, base=2) * parse(Int, co2, base=2)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day3

if abspath(PROGRAM_FILE) == @__FILE__
    Day3.main()
end
