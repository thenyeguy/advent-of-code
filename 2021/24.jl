module Day24

using Combinatorics

#
# Data loading:
function load_data()
    return collect(eachline(joinpath(@__DIR__, "24.txt")))
end

#
# Common code:
struct ALU
    registers::Dict{String, Int}
end

ALU() = ALU(Dict("w"=>0, "x"=>0, "y"=>0, "z"=>0))

read(alu, r) = r in ("w", "x", "y", "z") ? alu.registers[r] : parse(Int, r)

function run(program, inputs)
    alu = ALU()
    inputs = deepcopy(inputs)
    for line in program
        args = split(line)
        cmd = args[1]
        dst = args[2]
        src = length(args) > 2 ? args[3] : missing
        if cmd == "inp"
            alu.registers[dst] = popfirst!(inputs)
        elseif cmd == "add"
            alu.registers[dst] += read(alu, src)
        elseif cmd == "mul"
            alu.registers[dst] *= read(alu, src)
        elseif cmd == "div"
            alu.registers[dst] = div(read(alu, dst), read(alu, src), RoundToZero)
        elseif cmd == "mod"
            alu.registers[dst] %= read(alu, src)
        elseif cmd == "eql"
            alu.registers[dst] = read(alu, dst) == read(alu, src)
        end
    end
    return alu.registers
end

function decode(compute_digits)
    # The program is a stack, and constants in it pair up inputs numbers.
    # See 24_notes.txt for the reverse engineering of the stack.
    chk = [10, 11, 14, 13, -6, -14, 14, 13, -8, -15, 10, -11, -13, -4]
    sum = [ 1,  9, 12,  6,  9,  15,  7, 12, 15,   3,  6,   2,  10, 12]

    serial = zeros(Int, 14)
    for (l,r) in [(4,5), (3,6), (8,9), (7,10), (11,12), (2,13), (1,14)]
        diff = sum[l]+chk[r]
        serial[l], serial[r] = compute_digits(diff)
    end
    return parse(Int, join(serial))
end

function validate(monad, serial)
    input = parse.(Int, split(string(serial), ""))
    registers = run(monad, input)
    return registers["z"] == 0
end

#
# Part A:
function part_a(data)
    largest_pair(diff) = (diff > 0) ? (9-diff, 9) : (9, 9+diff)
    serial = decode(largest_pair)
    @assert validate(data, serial)
    return serial
end

#
# Part B:
function part_b(data)
    smallest_pair(diff) = (diff > 0) ? (1, 1+diff) : (1-diff, 1)
    serial = decode(smallest_pair)
    @assert validate(data, serial)
    return serial
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day24

if abspath(PROGRAM_FILE) == @__FILE__
    Day24.main()
end
