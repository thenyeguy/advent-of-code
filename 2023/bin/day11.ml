open Utils
open Utils.List.Infix

(*
 * Types
 *)
type coord = int * int (* row * col *)

let manhattan_distance ((r1, c1) : coord) ((r2, c2) : coord) : int =
  abs (r1 - r2) + abs (c1 - c2)

(*
 * Parse input
 *)
let puzzle_input () = Io.read_lines "data/11.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let find_stars ?(expansion : int = 2) (map : char Matrix.t) : coord list =
  (* Convert to a bool matrix. *)
  let empties = Matrix.map (( = ) '.') map in
  (* Expand empty rows to create a new coordinate row/col mapping. *)
  let expand_index i empty = ((i + if empty then expansion else 1), i) in
  let expand_indices = List.fold_left_map expand_index 0 in
  let _, row_indices = Matrix.fold_rows ( && ) true empties |> expand_indices in
  let _, col_indices = Matrix.fold_cols ( && ) true empties |> expand_indices in
  let coords = ref [] in
  (* Find all stars and apply the mapping. *)
  let add_star row col empty =
    if not empty then
      coords := (List.nth row_indices row, List.nth col_indices col) :: !coords
  in
  Matrix.iteri add_star empties;
  !coords

let part_one (map : char Matrix.t) : int =
  map |> find_stars |> List.combinations
  ||> Pair.apply manhattan_distance
  |> List.sum

(*
 * Part 2
 *)
let part_two (map : char Matrix.t) : int =
  map
  |> find_stars ~expansion:1000000
  |> List.combinations
  ||> Pair.apply manhattan_distance
  |> List.sum

(*
 * Main
 *)
let _ = Runner.main (puzzle_input ()) part_one part_two
