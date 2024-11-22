module Day4

function load_data()
    data = split(read(joinpath(@__DIR__, "data", "04.txt"), String), "\n\n")
    numbers = parse.(Int, split(data[1], ","))
    boards = map(b -> reshape(parse.(Int, split(b)), 5, 5), data[2:end])
    return numbers, boards
end

#
# Implement BingoGame Logic
struct BingoGame
    board::Matrix{Int}
    marked::Matrix{Bool}
end

BingoGame(board) = BingoGame(board, zeros(Bool, 5, 5))

function mark!(game, n)
    for i in 1:5
        for j in 1:5
            if game.board[i,j] == n
                game.marked[i,j] = true
            end
        end
    end
end

function has_won(game)
    for i in 1:5
        if sum(game.marked[i,:]) == 5 || sum(game.marked[:,i]) == 5
            return true
        end
    end
    return false
end

#
# Part A:
function score(game, n)
    s = 0
    for (num, marked) in zip(game.board, game.marked)
        s += num * !marked
    end
    return s * n
end

function part_a((numbers, boards))
    games = BingoGame.(boards)
    for n in numbers
        for game in games
            mark!(game, n)
            if has_won(game)
                return score(game, n)
            end
        end
    end
end

#
# Part B:
function part_b((numbers, boards))
    games = BingoGame.(boards)
    active = Set(games)
    for n in numbers
        for game in games
            mark!(game, n)
            if game in active && has_won(game)
                delete!(active, game)
                if isempty(active)
                    return score(game, n)
                end
            end
        end
    end
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day4

if abspath(PROGRAM_FILE) == @__FILE__
    Day4.main()
end
