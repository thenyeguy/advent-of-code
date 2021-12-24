module Day23

using DataStructures

#
# Data loading:
function load_data()
    lines = collect(eachline(joinpath(@__DIR__, "23.txt")))
    return map(i -> getindex.(lines[4:-1:3], i), [4, 6, 8, 10])
end

#
# Common code:

# ┌───────────┐
# │12 4 6 8 AB│
# └─┐ ╷ ╷ ╷ ┌─┘
#   │1│2│3│4│
#   └─┴─┴─┴─┘
const COSTS = Dict('A'=>1, 'B'=>10, 'C'=>100, 'D'=>1000)
const DESTINATIONS = Dict('A'=>1, 'B'=>2, 'C'=>3, 'D'=>4)
const ROOM_POSITIONS = Dict(1=>3, 2=>5, 3=>7, 4=>9)

struct Burrow
    hallway::Array{Char}
    rooms::Array{Array{Char}}
end

Burrow(rooms) = Burrow(collect("           "), rooms)

function burrow_id(b, depth)
    return uppercase(reduce(*,b.hallway) *
                     mapreduce(r -> rpad(reduce(*,r), depth), *, b.rooms))
end

amphipods_eq(l,r) = uppercase(l) == uppercase(r)
is_valid_hallway(h) = !(h in values(ROOM_POSITIONS))
occupied(amphipod) = amphipod != ' '
steps_between(room, hallway) = abs(ROOM_POSITIONS[room] - hallway) + 1
steps_into(room, depth) = depth - length(room)
steps_out_of(room, depth) = depth-1 - length(room)

function visible_hallways(burrow, r)
    midpoint = ROOM_POSITIONS[r]
    min_i, max_i = midpoint, midpoint
    for outer min_i in midpoint:-1:1
        occupied(burrow.hallway[min_i]) && break
    end
    for outer max_i in midpoint:11
        occupied(burrow.hallway[max_i]) && break
    end
    return filter(is_valid_hallway, min_i:max_i)
end

function move_into_room(burrow, h, r)
    new_hallway = deepcopy(burrow.hallway)
    new_rooms = deepcopy(burrow.rooms)
    push!(new_rooms[r], lowercase(new_hallway[h]))
    new_hallway[h] = ' '
    return Burrow(new_hallway, new_rooms)
end

function move_into_hallway(burrow, r, h)
    new_hallway = deepcopy(burrow.hallway)
    new_rooms = deepcopy(burrow.rooms)
    new_hallway[h] = pop!(new_rooms[r])
    return Burrow(new_hallway, new_rooms)
end

function possible_moves(burrow, depth)
    moves = []
    for r in 1:4
        room = burrow.rooms[r]
        for h in visible_hallways(burrow, r)
            if occupied(burrow.hallway[h])
                # Try to move into room...
                amphipod = burrow.hallway[h]
                # Room is correct destination:
                DESTINATIONS[amphipod] == r || continue
                # Room is full:
                length(room) < depth || continue
                # Room contains the wrong amphipod
                any(a -> !amphipods_eq(a, amphipod), room) && continue

                steps = steps_between(r,h) + steps_into(room, depth)
                cost = steps * COSTS[amphipod]
                push!(moves, (cost, move_into_room(burrow, h, r)))
            elseif !isempty(room)
                # Try to move out of room...
                amphipod = room[end]
                # Amphipod hasn't moved:
                islowercase(amphipod) && continue

                steps = (steps_between(r,h) + steps_out_of(room, depth))
                cost = steps * COSTS[amphipod]
                push!(moves, (cost, move_into_hallway(burrow, r, h)))
            end
        end
    end
    return moves
end

function is_organized(burrow, depth)
    for (room,amphipod) in zip(burrow.rooms, ['A', 'B', 'C', 'D'])
        length(room) == depth || return false
        all(a -> amphipods_eq(a,amphipod), room) || return false
    end
    return true
end

function organize_amphipods(starting_burrow; depth=2)
    frontier = PriorityQueue(starting_burrow=>0)
    seen = Set()
    while !isempty(frontier)
        (burrow, cost) = peek(frontier)
        is_organized(burrow, depth) && return cost
        delete!(frontier, burrow)

        id = burrow_id(burrow, depth)
        id in seen && continue
        push!(seen, id)

        for (cost_delta, move) in possible_moves(burrow, depth)
            enqueue!(frontier, move, cost+cost_delta)
        end
    end
end


#
# Part A:
function part_a(data)
    return organize_amphipods(Burrow(data))
end

#
# Part B:
function part_b(data)
    new_rows = [['D','D'],['B','C'],['A','B'],['C','A']]
    new_data = map(z -> vcat(z[1][1], z[2], z[1][2]), zip(data, new_rows))
    return organize_amphipods(Burrow(new_data); depth=4)
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day23

if abspath(PROGRAM_FILE) == @__FILE__
    Day23.main()
end
