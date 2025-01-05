open Utils

(*
 * Parse input
 *)
type segment_t = dir * int

let parse_segment (s : string) : segment_t =
  let dir = String.get s 0 in
  let l = String.length s in
  let steps = String.sub s 1 (l - 1) in
  (Coord.dir_of_char dir, int_of_string steps)

let parse (lines : string list) : segment_t list * segment_t list =
  let [ a; b ] = lines in
  let parse_inner = String.split_on_char ',' >> List.map parse_segment in
  (parse_inner a, parse_inner b)

let puzzle_input = Io.read_lines "2019/data/03.txt" |> parse

(*
 * Part 1
 *)
let crawl (segments : segment_t list) : coord list =
  let acc (start, points) (dir, steps) =
    let step steps = Coord.step ~steps start dir in
    let start' = step steps in
    let points' = List.range steps ||> step in
    (start', points @ points')
  in
  List.fold_left acc ((0, 0), []) segments |> snd

let intersections (left : coord list) (right : coord list) : Coord.Set.t =
  Coord.Set.(inter (of_list left) (of_list right) |> filter (( <> ) (0, 0)))

let closest_point (left : coord list) (right : coord list) : int =
  intersections left right |> Coord.Set.to_list
  ||> Coord.manhattan_distance (0, 0)
  |> List.min

let part_one (a, b) = closest_point (crawl a) (crawl b)

(*
 * Part 2
 *)
let soonest_point (left : coord list) (right : coord list) : int =
  let time_delay i =
    let delay ls = List.find_index (( = ) i) ls |> Option.get in
    delay left + delay right
  in
  intersections left right |> Coord.Set.to_list ||> time_delay |> List.min

let part_two (a, b) = soonest_point (crawl a) (crawl b)

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
