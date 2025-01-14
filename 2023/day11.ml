open Utils

(*
 * Parse input
 *)
let puzzle_input () = Io.read_lines "2023/data/11.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let find_stars ?(expansion : int = 2) (map : char matrix) : coord list =
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

let part_one (map : char matrix) : int =
  map |> find_stars |> List.combinations
  ||> Pair.apply Coord.manhattan_distance
  |> List.sum

(*
 * Part 2
 *)
let part_two (map : char matrix) : int =
  map
  |> find_stars ~expansion:1000000
  |> List.combinations
  ||> Pair.apply Coord.manhattan_distance
  |> List.sum

(*
 * Main
 *)
let _ = Runner.main (puzzle_input ()) part_one part_two
