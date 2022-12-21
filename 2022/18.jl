module Day18

using DataStructures

function load_data()
    return map.(l -> parse.(Int, l), split.(eachline(joinpath(@__DIR__, "18.txt")), ","))
end

# Returns the center points of each cube's face.
function get_faces(cube)
    x,y,z = cube
    return [
        [x, y, z-0.5], [x, y, z+0.5],
        [x, y-0.5, z], [x, y+0.5, z],
        [x-0.5, y, z], [x+0.5, y, z],
    ]
end

function exterior_faces(cubes)
    faces = DefaultDict(0)
    for cube in cubes
        for face in get_faces(cube)
            faces[face] += 1
        end
    end
    return filter(e-> e.second == 1, faces) |> keys |> collect
end

function part_a(data)
    return data |> exterior_faces |> length
end

function neighbors(cube)
    x,y,z = cube
    return [[x-1,y,z], [x+1,y,z], [x,y-1,z], [x,y+1,z], [x,y,z-1], [x,y,z+1]]
end

function search_for_faces(cubes)
    xs = minimum(getindex.(cubes, 1))-1:maximum(getindex.(cubes,1))+1
    ys = minimum(getindex.(cubes, 2))-1:maximum(getindex.(cubes,2))+1
    zs = minimum(getindex.(cubes, 3))-1:maximum(getindex.(cubes,3))+1

    faces = Set()
    visited = Set()
    frontier = [[0,0,0]]
    while !isempty(frontier)
        cube = pop!(frontier)
        cube in visited && continue
        push!(visited, cube)
        for n in neighbors(cube)
            if n[1] in xs && n[2] in ys && n[3] in zs
                if n in cubes
                    push!(faces, (cube + n) / 2)
                else
                    push!(frontier, n)
                end
            end
        end
    end
    return faces
end

function part_b(data)
    return data |> search_for_faces |> length
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day18

if abspath(PROGRAM_FILE) == @__FILE__
    Day18.main()
end
