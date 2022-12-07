module Day6

function load_data()
    return read(joinpath(@__DIR__, "06.txt"), String)
end

function find_marker(signal, marker_len)
    for i in 1:length(signal)
        if length(Set(signal[i:i+marker_len-1])) == marker_len
            return i+marker_len-1
        end
    end
end

function part_a(data)
    return find_marker(data, 4)
end

function part_b(data)
    return find_marker(data, 14)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day6

if abspath(PROGRAM_FILE) == @__FILE__
    Day6.main()
end
