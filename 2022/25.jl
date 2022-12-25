module Day25

function load_data()
    return collect(eachline(joinpath(@__DIR__, "25.txt")))
end

function snafu_to_decimal(snafu)
    DIGITS = Dict('='=>-2, '-'=>-1, '0'=>0, '1'=>1, '2'=>2)

    decimal = 0
    place = 1
    for digit in reverse(snafu)
        decimal += place*DIGITS[digit]
        place *= 5
    end
    return decimal
end

function decimal_to_snafu(decimal)
    DIGITS = ['0', '1', '2', '=', '-']

    snafu = ""
    while decimal != 0
        m = mod(decimal, 5)
        snafu *= DIGITS[m+1]
        if m > 2
            decimal += 5
        end
        decimal ÷= 5
    end
    return reverse(snafu)
end

function part_a(data)
    return data .|> snafu_to_decimal |> sum |> decimal_to_snafu
end

function part_b(data)
    return "Merry Christmas 🎄"
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day25

if abspath(PROGRAM_FILE) == @__FILE__
    Day25.main()
end
