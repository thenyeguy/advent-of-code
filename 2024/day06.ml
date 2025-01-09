open Utils

module VecSet = Set.Make (struct
  type t = Coord.t * Coord.dir

  let compare = compare
end)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/06.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let starting_position (m : char Matrix.t) : Coord.t * Coord.dir =
  (Matrix.find (( = ) '^') m |> Option.get, Coord.Up)

let rec step (m : char Matrix.t) (pos : Coord.t) (dir : Coord.dir) :
    (Coord.t * Coord.dir) option =
  let pos' = Coord.step pos dir in
  match Matrix.get_opt m pos' with
  | Some '#' -> step m pos (Coord.turn_right dir)
  | Some _ -> Some (Coord.step pos dir, dir)
  | None -> None

let trace_path (m : char Matrix.t) : Coord.Set.t =
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
let add_obstacle (m : char Matrix.t) (pos : Coord.t) : char Matrix.t =
  let m' = Matrix.copy m in
  Matrix.set m' pos '#';
  m'

let has_cycle (m : char Matrix.t) (pos : Coord.t) (dir : Coord.dir) : bool =
  let rec trace seen pos dir =
    match step m pos dir with
    | Some (pos', dir') ->
        if VecSet.find_opt (pos', dir') seen |> Option.is_some then true
        else trace (VecSet.add (pos', dir') seen) pos' dir'
    | None -> false
  in
  trace (VecSet.singleton (pos, dir)) pos dir

let find_cycles (m : char Matrix.t) : Coord.Set.t =
  let pos, dir = starting_position m in
  let has_cycle' p = has_cycle (add_obstacle m p) pos dir in
  trace_path m |> Coord.Set.remove pos |> Coord.Set.filter has_cycle'

let part_two input = find_cycles input |> Coord.Set.cardinal

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
