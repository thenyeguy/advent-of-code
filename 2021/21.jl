module Day21

using Combinatorics
using DataStructures

#
# Data loading:
function load_data()
    return [2, 10]
end

#
# Common code:
mutable struct Player
    pos::Int
    score::Int
end

Player(starting_pos) = Player(starting_pos, 0)

function move(player, spots)
    player.pos = (player.pos + spots - 1)%10 + 1
    player.score += player.pos
end

other_player(i) = i == 1 ? 2 : 1

#
# Part A:
mutable struct DeterministicDie
    last_roll::Int
    num_rolls::Int
end

DeterministicDie() = DeterministicDie(100, 0)

function roll(die::DeterministicDie)
    die.num_rolls += 1
    die.last_roll = die.last_roll%100 + 1
    return die.last_roll
end

function play_deterministic(starting_positions)
    die = DeterministicDie()
    players = Player.(starting_positions)
    while true
        for (i, player) in enumerate(players)
            move(player, mapreduce(i->roll(die), +, 1:3))
            if player.score >= 1000
                return players[other_player(i)].score * die.num_rolls
            end
        end
    end
end

function part_a(data)
    return play_deterministic(data)
end

#
# Part B:
function play_dirac(starting_positions)
    possible_rolls = multiset_permutations(1:3, [3,3,3], 3) .|> sum |> counter

    total_wins = [0, 0]
    positions = zeros(Int, 10, 21, 10, 21)
    positions[starting_positions[1], 1, starting_positions[2], 1] = 1
    while true
        sum(positions) == 0 && break
        for i in 1:2
            new_positions = zeros(Int, 10, 21, 10, 21)
            for idx in CartesianIndices(new_positions)
                occurences = positions[idx]
                occurences > 0 || continue

                (p1,s1,p2,s2) = Tuple(idx)
                for (roll, freq) in possible_rolls
                    pos_freq = occurences * freq
                    if i == 1
                        new_p1 = (p1 + roll - 1)%10 + 1
                        new_s1 = s1 + new_p1
                        if new_s1 > 21
                            total_wins[i] += pos_freq
                        else
                            new_positions[new_p1, new_s1, p2, s2] += pos_freq
                        end
                    else
                        new_p2 = (p2 + roll - 1)%10 + 1
                        new_s2 = s2 + new_p2
                        if new_s2 > 21
                            total_wins[i] += pos_freq
                        else
                            new_positions[p1, s1, new_p2, new_s2] += pos_freq
                        end
                    end
                end
            end
            positions = new_positions
        end
    end
    return total_wins
end

function part_b(data)
    return play_dirac(data) |> maximum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day21

if abspath(PROGRAM_FILE) == @__FILE__
    Day21.main()
end
