let read_file (parse: string -> 'a) (fname: string): ('a list) =
    let rec read_lines (ch : in_channel) : (string list) =
        try
            let line = input_line ch in
            line :: read_lines ch
        with
        | End_of_file -> [] in
    let ic = open_in fname in
    let lines = read_lines ic in
    close_in ic;
    List.map parse lines

type color = Red | Green | Blue
type game = { id: int; cubes: (int*color) list list }

let map_reduce (f: 'a -> 'b) (g: 'b -> 'b -> 'acc) (acc: 'acc) (l: 'a list): 'acc =
    List.map f l |> List.fold_left g acc

let parse_game (line: string) : game =
    let parse_id s  = Scanf.sscanf s "Game %d" (fun n -> n) in
    let parse_color s = match s with "red" -> Red | "green" -> Green | "blue" -> Blue in
    let parse_cube s = Scanf.sscanf s " %d %s" (fun n c -> (n, parse_color c)) in
    let parse_cube_set s = String.split_on_char ',' s |> List.map parse_cube in
    let parse_all_cubes s = String.split_on_char ';' s |> List.map parse_cube_set in
    let [id; all_cubes] = String.split_on_char ':' line in
    { id=parse_id id; cubes=parse_all_cubes all_cubes }

let is_game_possible (g: game): bool =
    let is_cube_possible (n,c: int*color): bool =
        match c with
            Red -> n <= 12
          | Green -> n <= 13
          | Blue -> n <= 14 in
    map_reduce is_cube_possible (&&) true (List.flatten g.cubes)

let fewest_cubes (g: game): (int*int*int) =
    let cube_triple (n, c) =
        match c with Red -> (n, 0, 0) | Green -> (0, n, 0) | Blue -> (0, 0, n) in
    let max_triple (r1,g1,b1) (r2,g2,b2) = (max r1 r2, max g1 g2, max b1 b2) in
    map_reduce cube_triple max_triple (0,0,0) (List.flatten g.cubes)

let cube_power (g: game): int =
    let (r,g,b) = fewest_cubes g in r*g*b

let puzzle_input = read_file parse_game "02.txt"

let part_one (input: game list) =
    List.filter is_game_possible input |> map_reduce (fun g -> g.id) (+) 0

let part_two (input: game list) =
    map_reduce cube_power (+) 0 input
