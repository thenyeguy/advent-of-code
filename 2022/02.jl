module Day2

# A = rock     = X  ( = lose )
# B = paper    = Y  ( = draw )
# C = scissors = Z  ( = win )

function load_data()
    return split.(eachline(joinpath(@__DIR__, "data", "02.txt")), " ")
end

function score(round)
    them, us = round
    score = if us == "X" 1 elseif us == "Y" 2 else 3 end
    if them == "A"
        score += if us == "X" 3 elseif us == "Y" 6 else 0 end
    elseif them == "B"
        score += if us == "X" 0 elseif us == "Y" 3 else 6 end
    else
        score += if us == "X" 6 elseif us == "Y" 0 else 3 end
    end
    return score
end

function part_a(data)
    return data .|> score |> sum
end

function choose_play(round)
    them, us = round
    play = if them == "A"
        if us == "X" "Z" elseif us == "Y" "X" else "Y" end
    elseif them == "B"
        if us == "X" "X" elseif us == "Y" "Y" else "Z" end
    else
        if us == "X" "Y" elseif us == "Y" "Z" else "X" end
    end
    return [them, play]
end

function part_b(data)
    return data .|> choose_play .|> score |> sum
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day2

if abspath(PROGRAM_FILE) == @__FILE__
    Day2.main()
end
