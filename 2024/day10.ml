open Utils
open Utils.List.Infix

(*
 * Parse input
 *)
let puzzle_input =
  Io.read_lines "2024/data/10.txt"
  |> Matrix.of_strings
  |> Matrix.map Char.digit_of_char

(*
 * Part 1
 *)
let find_trailheads (m : int Matrix.t) : Coord.t list =
  Matrix.find_all (( = ) 0) m

let neighbors (m : int Matrix.t) (c : Coord.t) =
  let h = Matrix.get m c in
  let is_valid c' = Matrix.get_opt m c' = Some (h + 1) in
  List.filter is_valid (Coord.adjacencies c)

let trail_score (m : int Matrix.t) (trailhead : Coord.t) : int =
  let rec dfs (seen : Coord.Set.t) (frontier : Coord.t list) : Coord.Set.t =
    match frontier with
    | [] -> seen
    | c :: cs ->
        if Coord.Set.find_opt c seen |> Option.is_some then
          (dfs [@tailcall]) seen cs
        else
          let seen' = Coord.Set.add c seen in
          let frontier' = neighbors m c @ cs in
          (dfs [@tailcall]) seen' frontier'
  in
  let count_peaks c count = if Matrix.get m c = 9 then count + 1 else count in
  let seen = dfs Coord.Set.empty [ trailhead ] in
  Coord.Set.fold count_peaks seen 0

let part_one input = find_trailheads input ||> trail_score input |> List.sum

(*
 * Part 2
 *)
let trail_rating (m : int Matrix.t) (trailhead : Coord.t) : int =
  let rec rating (c : Coord.t) : int =
    if Matrix.get m c = 9 then 1 else neighbors m c ||> rating |> List.sum
  in
  rating trailhead

let part_two input = find_trailheads input ||> trail_rating input |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
