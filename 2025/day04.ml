open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/04.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let is_accessible (m : char matrix) (c : coord) (v : char) : bool =
  let adjacent = Matrix.surrounding m c |> List.count (( = ) '@') in
  v = '@' && adjacent < 4

let count_accessible (m : char matrix) : int = Matrix.counti (is_accessible m) m
let part_one = count_accessible

(*
 * Part 2
 *)
let remove_paper (m : char matrix) : char matrix =
  let update r c v = if is_accessible m (r, c) v then '.' else v in
  Matrix.mapi update m

let rec remove_all_paper (m : char matrix) : char matrix =
  let m' = remove_paper m in
  if count_accessible m' > 0 then remove_all_paper m' else m

let part_two input =
  let total = Matrix.count (( = ) '@') input in
  let remaining = remove_all_paper input |> Matrix.count (( = ) '@') in
  total - remaining

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
