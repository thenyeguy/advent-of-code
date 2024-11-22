module Day2

struct Command
    dir::String
    units::Int
end

function parse_command(line)
    dir, units = split(line, " ")
    return Command(dir, parse(Int, units))
end

function load_commands()
    return parse_command.(eachline(joinpath(@__DIR__, "data", "02.txt")))
end

function part_a(commands)
    pos = 0
    depth = 0
    for cmd in commands
        if cmd.dir == "forward"
            pos += cmd.units
        elseif cmd.dir == "down"
            depth += cmd.units
        elseif cmd.dir == "up"
            depth -= cmd.units
        end
    end
    return pos * depth
end

function part_b(commands)
    pos = 0
    aim = 0
    depth = 0
    for cmd in commands
        if cmd.dir == "forward"
            pos += cmd.units
            depth += aim * cmd.units
        elseif cmd.dir == "down"
            aim += cmd.units
        elseif cmd.dir == "up"
            aim -= cmd.units
        end
    end
    return pos * depth
end

function main()
    commands = load_commands()
    println("Part A: ", part_a(commands))
    println("Part B: ", part_b(commands))
end

end # module Day2

if abspath(PROGRAM_FILE) == @__FILE__
    Day2.main()
end
