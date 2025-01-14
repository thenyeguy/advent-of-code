open Utils

module VecSet = Set.Make (struct
  type t = coord * dir

  let compare = compare
end)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/06.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let starting_position (m : char matrix) : coord * dir =
  (Matrix.find (( = ) '^') m |> Option.get, Coord.Up)

let rec step (m : char matrix) (pos : coord) (dir : dir) : (coord * dir) option
    =
  let pos' = Coord.step pos dir in
  match Matrix.get_opt m pos' with
  | Some '#' -> step m pos (Coord.turn_right dir)
  | Some _ -> Some (Coord.step pos dir, dir)
  | None -> None

let trace_path (m : char matrix) : Coord.Set.t =
  let rec trace seen pos dir =
    match step m pos dir with
    | Some (pos', dir') -> trace (Coord.Set.add pos' seen) pos' dir'
    | None -> seen
  in
  let pos, dir = starting_position m in
  trace (Coord.Set.singleton pos) pos dir

let part_one input = trace_path input |> Coord.Set.cardinal

(*
 * Part 2
 *)
let add_obstacle (m : char matrix) (pos : coord) : char Matrix.t =
  let m' = Matrix.copy m in
  Matrix.set m' pos '#';
  m'

let has_cycle (m : char matrix) (pos : coord) (dir : dir) : bool =
  let rec trace seen pos dir =
    match step m pos dir with
    | Some (pos', dir') ->
        if VecSet.find_opt (pos', dir') seen |> Option.is_some then true
        else trace (VecSet.add (pos', dir') seen) pos' dir'
    | None -> false
  in
  trace (VecSet.singleton (pos, dir)) pos dir

let find_cycles (m : char matrix) : Coord.Set.t =
  let pos, dir = starting_position m in
  let has_cycle' p = has_cycle (add_obstacle m p) pos dir in
  trace_path m |> Coord.Set.remove pos |> Coord.Set.filter has_cycle'

let part_two input = find_cycles input |> Coord.Set.cardinal

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
