open Utils

type color = Red | Green | Blue
type game = { id : int; cubes : (int * color) list list }

let parse_game (line : string) : game =
  let parse_id s = Scanf.sscanf s "Game %d" (fun n -> n) in
  let parse_color s =
    match s with "red" -> Red | "green" -> Green | "blue" -> Blue
  in
  let parse_cube s = Scanf.sscanf s " %d %s" (fun n c -> (n, parse_color c)) in
  let parse_cube_set s = String.split_on_char ',' s |> List.map parse_cube in
  let parse_all_cubes s =
    String.split_on_char ';' s |> List.map parse_cube_set
  in
  let [ id; all_cubes ] = String.split_on_char ':' line in
  { id = parse_id id; cubes = parse_all_cubes all_cubes }

let is_game_possible (g : game) : bool =
  let is_cube_possible ((n, c) : int * color) : bool =
    match c with Red -> n <= 12 | Green -> n <= 13 | Blue -> n <= 14
  in
  g.cubes |> List.flatten ||> is_cube_possible |> List.all

let fewest_cubes (g : game) : int * int * int =
  let cube_triple (n, c) =
    match c with Red -> (n, 0, 0) | Green -> (0, n, 0) | Blue -> (0, 0, n)
  in
  let max_triple (r1, g1, b1) (r2, g2, b2) =
    (max r1 r2, max g1 g2, max b1 b2)
  in
  g.cubes |> List.flatten ||> cube_triple |> List.fold_left max_triple (0, 0, 0)

let cube_power (g : game) : int =
  let r, g, b = fewest_cubes g in
  r * g * b

let puzzle_input = Io.read_lines "2023/data/02.txt" ||> parse_game

let part_one (input : game list) =
  input |> List.filter is_game_possible ||> (fun g -> g.id) |> List.sum

let part_two (input : game list) = input ||> cube_power |> List.sum
let _ = Runner.main puzzle_input part_one part_two
