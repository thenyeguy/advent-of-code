module Day12

#
# Data loading:
function load_data()
    return collect(eachline(joinpath(@__DIR__, "data", "12.txt")))
end

#
# Common code:
struct Cave
    label::String
    big::Bool
end

Cave(label) = Cave(label, isuppercase(label[1]))

struct Graph
    edges::Dict{Cave,Set{Cave}}
end

function build_graph(data)
    edges = Dict()
    function add_edge!(c1, c2)
        if c1.label == "end" || c2.label == "start"
            return
        elseif haskey(edges, c1)
            push!(edges[c1], c2)
        else
            edges[c1] = Set([c2])
        end
    end

    for edge in data
        c1,c2 = Cave.(split(edge, "-"))
        add_edge!(c1, c2)
        add_edge!(c2, c1)
    end
    return Graph(edges)
end

struct Path
    caves::Array{Cave}
    allow_repeat::Bool
end

function is_valid_cave(path::Path, cave::Cave)
    return cave.big || path.allow_repeat || !(cave in path.caves)
end

function extend_path(path::Path, cave::Cave)
    allow_repeat = path.allow_repeat && (cave.big || !(cave in path.caves))
    return Path(vcat(path.caves, cave), allow_repeat)
end

function find_paths(graph, allow_repeat=false)
    frontier = [Path([Cave("start")], allow_repeat)]
    seen = Set()
    paths = []
    while !isempty(frontier)
        path = pop!(frontier)
        for cave in graph.edges[path.caves[end]]
            if !is_valid_cave(path, cave)
                continue
            end

            new_path = extend_path(path, cave)
            if cave.label == "end"
                push!(paths, new_path)
            elseif !(new_path in seen)
                push!(frontier, new_path)
                push!(seen, new_path)
            end
        end
    end
    return paths
end

#
# Part A:
function part_a(data)
    return data |> build_graph |> find_paths |> length
end

#
# Part B:
function part_b(data)
    return data |> build_graph |> g -> find_paths(g, true) |> length
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day12

if abspath(PROGRAM_FILE) == @__FILE__
    Day12.main()
end
