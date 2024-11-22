module Day13

function parse_line(line)
    return line |> Meta.parse |> eval
end

function load_data()
    return filter(l -> !isnothing(l), parse_line.(eachline(joinpath(@__DIR__, "data", "13.txt"))))
end

@enum Comparison less equal greater

function compare_int(left, right)
    if left < right
        return less
    elseif left > right
        return greater
    else
        return equal
    end
end

function compare(left, right)
    if isa(left, Int) && isa(right, Int)
        return compare_int(left, right)
    else
        if isa(left, Int)
            left = [left]
        end
        if isa(right, Int)
            right = [right]
        end
        for i in 1:min(length(left), length(right))
            result = compare(left[i], right[i])
            if result != equal
                return result
            end
        end
        return compare_int(length(left), length(right))
    end
end

function compare_packets(packets)
    sum = 0
    for i in 1:(length(packets)÷2)
        result = compare(packets[2*i-1], packets[2*i])
        if result == less
            sum += i
        end
    end
    return sum
end

function part_a(data)
    return data |> compare_packets
end

function find_divider(divider, packets)
    for i in 1:length(packets)
        if packets[i] == divider
            return i
        end
    end
end

function part_b(data)
    packets = deepcopy(data)
    push!(packets, [[2]])
    push!(packets, [[6]])
    sort!(packets, lt=(l,r) -> compare(l,r)==less)
    first = find_divider([[2]], packets)
    second = find_divider([[6]], packets)
    return first * second
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day13

if abspath(PROGRAM_FILE) == @__FILE__
    Day13.main()
end
