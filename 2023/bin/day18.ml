open Utils
open Utils.List.Infix

(*
 * Types
 *)
type trench = { dir : Coord.dir; length : int; color : string }

(*
 * Parse input
 *)
let parse_trench (line : string) : trench =
  let parse_dir c =
    Coord.(match c with 'U' -> Up | 'D' -> Down | 'L' -> Left | 'R' -> Right)
  in
  let build_trench dir length color = { dir = parse_dir dir; length; color } in
  Scanf.sscanf line "%c %d (#%[0-9a-z])" build_trench

let puzzle_input = Io.read_lines "data/18.txt" ||> parse_trench

(*
 * Part 1
 *)
(* Get a list of vertices for the trenches. *)
let dig_trenches (dig_plan : trench list) : Coord.t list =
  let next_vertex acc trench =
    (Coord.step ~steps:trench.length trench.dir acc, acc)
  in
  List.fold_left_map next_vertex (0, 0) dig_plan |> Pair.right

let trench_area (vertices : Coord.t list) : int =
  let perimeter (vertices : Coord.t list) : int =
    let rest = vertices |> List.pair_map Coord.manhattan_distance |> List.sum in
    let last =
      Coord.manhattan_distance (List.hd vertices) (List.last vertices)
    in
    rest + last
  in
  let shoelace_area (vertices : Coord.t list) : int =
    let half n = n / 2 in
    let det (r1, c1) (r2, c2) = (r1 * c2) - (r2 * c1) in
    vertices |> List.pair_map det |> List.sum |> half
  in
  (* Computing the interior area with shoelace includes half the perimeter, as
   * the coordinates are effectively inclusive on the left but not on the right.
   * Bottom right corner must be counted twice (both horizontal and vertical).
   *)
  shoelace_area vertices + (perimeter vertices / 2) + 1

let part_one (input : trench list) : int = input |> dig_trenches |> trench_area

(*
 * Part 2
 *)
let extract_trench_from_color (trench : trench) : trench =
  let length_str = "0x" ^ String.sub trench.color 0 5 in
  let dir =
    Coord.(
      match String.get trench.color 5 with
      | '0' -> Right
      | '1' -> Down
      | '2' -> Left
      | '3' -> Up)
  in
  { length = int_of_string length_str; dir; color = trench.color }

let part_two (input : trench list) : int =
  input ||> extract_trench_from_color |> dig_trenches |> trench_area

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
