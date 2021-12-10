module Day10

using Statistics

#
# Data loading:
function load_data()
    return collect(eachline(joinpath(@__DIR__, "10.txt")))
end

#
# Common code:
struct Corruption
    illegal_char::Char
end

struct Incomplete
    open_chunks::Array{Char}
end

function detect_errors(line)
    function matches(open, close)
        (open == '(' && close == ')') || (open == '[' && close == ']') ||
        (open == '{' && close == '}') || (open == '<' && close == '>')
    end

    chunks = []
    for c in line
        if c in ['(', '[', '{', '<']
            push!(chunks, c)
        else
            open = pop!(chunks)
            if !matches(open, c)
                return Corruption(c)
            end
        end
    end
    if !isempty(chunks)
        return Incomplete(chunks)
    end
end

#
# Part A:
function part_a(data)
    function score(line)
        if     line == Corruption(')') 3
        elseif line == Corruption(']') 57
        elseif line == Corruption('}') 1197
        elseif line == Corruption('>') 25137
        else                           0
        end
    end

    return data .|> detect_errors .|> score |> sum
end

#
# Part B:
function part_b(data)
    function score(line)
        if !isa(line, Incomplete)
            return missing
        end
        values = Dict('('=>1, '['=>2, '{'=>3, '<'=>4)
        return mapfoldr(c->values[c], (c,s) -> 5*s+c, line.open_chunks)
    end

    return data .|> detect_errors .|> score |> skipmissing |> median |> Int
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day10

if abspath(PROGRAM_FILE) == @__FILE__
    Day10.main()
end
