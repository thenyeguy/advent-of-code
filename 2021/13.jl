module Day13

#
# Data loading:
struct Fold
    axis::String
    pos::Int
end

function load_data()
    f = read(joinpath(@__DIR__, "data", "13.txt"), String)
    (dots_str, folds_str) = split(f, "\n\n")

    dots = map(l -> tuple(parse.(Int, split(l, ","))...), split(dots_str))
    folds_m = match.(r"(\w)=(\d+)", split(folds_str, "\n"))
    folds = map(m -> Fold(m[1], parse(Int, m[2])), folds_m)
    return (dots, folds)
end

#
# Common code:
function fold(dots, folds)
    (max_x, max_y) = reduce(((x1,y1), (x2,y2)) -> (max(x1,x2), max(y1,y2)), dots)
    paper = zeros(Bool, (max_x+1, max_y+1))
    for (x,y) in dots
        paper[x+1,y+1] = true
    end

    for fold in folds
        if fold.axis == "y" paper = paper' end
        left = paper[1:fold.pos, :]
        right = paper[end:-1:fold.pos+2, :]
        paper = left .| right
        if fold.axis == "y" paper = paper' end
    end
    return paper
end

#
# Part A:
function part_a((dots, folds))
    return fold(dots, folds[1:1]) |> sum
end

#
# Part B:
function render(paper)
    paper = paper'
    text = ""
    for x in 1:2:size(paper, 1)
        line = ""
        for y in 1:2:size(paper, 2)
            block = paper[x:x+1, y:y+1]
            if     block == [1 1; 1 1] line *= "██" #"█"
            elseif block == [1 1; 1 0] line *= "█▀" #"▛"
            elseif block == [1 1; 0 1] line *= "▀█" #"▜"
            elseif block == [1 0; 1 1] line *= "█▄" #"▙"
            elseif block == [0 1; 1 1] line *= "▄█" #"▟"
            elseif block == [1 0; 1 0] line *= "█ " #"▌"
            elseif block == [0 1; 0 1] line *= " █" #"▐"
            elseif block == [1 1; 0 0] line *= "▀▀" #"▀"
            elseif block == [0 0; 1 1] line *= "▄▄" #"▄"
            elseif block == [1 0; 0 1] line *= "▀▄" #"▚"
            elseif block == [0 1; 1 0] line *= "▄▀" #"▞"
            elseif block == [1 0; 0 0] line *= "▀ " #"▘"
            elseif block == [0 1; 0 0] line *= " ▀" #"▝"
            elseif block == [0 0; 1 0] line *= "▄ " #"▖"
            elseif block == [0 0; 0 1] line *= " ▄" #"▗"
            else                       line *= "  " #" "
            end
        end
        text *= line * "\n"
    end
    return text
end

function part_b((dots, folds))
    return fold(dots, folds) |> render
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", replace(part_b(data), "\n"=>"\n        "))
end

end # module Day13

if abspath(PROGRAM_FILE) == @__FILE__
    Day13.main()
end
