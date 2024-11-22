module Day8

#
# Data loading:
Code = Set{Char}
struct Entry
    signal_patterns::Array{Code}
    output_values::Array{Code}
end

function parse_entry(str::AbstractString)
    function parse_codes(s)
        return map(s -> Set(collect(s)), split(s))
    end
    (pattern, value) = split(str, " | ")
    return Entry(parse_codes(pattern), parse_codes(value))
end

function Base.show(io::IO, c::Code)
    show(io, prod(c))
end

function load_data()
    return parse_entry.(eachline(joinpath(@__DIR__, "data", "08.txt")))
end

#
# Part A:
kEasySegments = Dict(2=>'1', 3=>'7', 4=>'4', 7=>'8')

function part_a(data)
    function count_easy_segments(entry)
        return count(v -> length(v) in kEasySegments.keys, entry.output_values)
    end
    return mapreduce(count_easy_segments, +, data)
end

#
# Part B:
function decode_entry(entry)
    # Identify codes in signal patterns.
    codes = Dict()
    one = missing
    four = missing
    for code in sort(entry.signal_patterns, by=c->length(c))
        l = length(code)
        if l in kEasySegments.keys
            digit = kEasySegments[l]
            if digit == '1'
                one = code
            elseif digit == '4'
                four = code
            end
            codes[code] = digit
        elseif l == 5
            if length(intersect(code, one)) == 2
                codes[code] = '3'
            elseif length(intersect(code, four)) == 2
                codes[code] = '2'
            else
                codes[code] = '5'
            end
        elseif l == 6
            if length(intersect(code, four)) == 4
                codes[code] = '9'
            elseif length(intersect(code, one)) == 2
                codes[code] = '0'
            else
                codes[code] = '6'
            end
        end
    end

    # Decode output value
    decoded = map(c -> codes[c], entry.output_values)
    return parse(Int, prod(decoded))
end

function part_b(data)
    return mapreduce(decode_entry, +, data)
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
