module Day1

function load_data()
    return parse.(Int, eachline(joinpath(@__DIR__, "data", "01.txt")))
end

function part_a(data)
    increases = 0
    for i in 1:length(data)-1
        if data[i] < data[i+1]
            increases += 1
        end
    end
    return increases
end

function part_b(data)
    increases = 0
    for i in 1:length(data)-3
        if data[i] < data[i+3]
            increases += 1
        end
    end
    return increases
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day1

if abspath(PROGRAM_FILE) == @__FILE__
    Day1.main()
end
