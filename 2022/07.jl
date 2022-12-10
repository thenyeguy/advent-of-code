module Day7

struct Directory
    name::String
    subdirectories::Dict{String,Directory}
    files::Dict{String,Int}
end

Directory(name) = Directory(name, Dict(), Dict())

function parse_directory_tree(lines)
    root = Directory("/")
    tree = []
    cwd = Nothing
    for line in lines
        cd = match(r"\$ cd (.+)", line)
        ls = match(r"(\d+) (.+)", line)
        if !isnothing(cd)
            dir = cd.captures[1]
            if dir == "/"
                tree = [root]
            elseif dir == ".."
                pop!(tree)
            else
                if !haskey(cwd.subdirectories, dir)
                    cwd.subdirectories[dir] = Directory(dir)
                end
                push!(tree, cwd.subdirectories[dir])
            end
            cwd = last(tree)
        elseif !isnothing(ls)
            size = parse(Int, ls.captures[1])
            name = ls.captures[2]
            cwd.files[name] = size
        end
    end
    return root
end

function load_data()
    return parse_directory_tree(eachline(joinpath(@__DIR__, "07.txt")))
end

function traverse(dir, fn)
    size = 0
    for (_, subdir) in dir.subdirectories
        size += traverse(subdir, fn)
    end
    for (_, file_size) in dir.files
        size += file_size
    end
    fn(dir, size)
    return size
end

function part_a(data)
    total_size = 0
    traverse(data, function (_, size)
        if size <= 100000
            total_size += size
        end
    end)
    return total_size
end

function part_b(data)
    sizes = []
    used_space = traverse(data, (d,s) -> push!(sizes, s))
    available_space = 70000000 - used_space
    required_space = 30000000 - available_space
    return filter(s -> s >= required_space, sizes) |> minimum
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
