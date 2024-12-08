open Utils
open Utils.Fn.Infix
open Utils.List.Infix
module CharMap = Map.Make (Char)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/08.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let find_antennas (m : char Matrix.t) : Coord.t list CharMap.t =
  let add_coord row col value = ((row, col), value) in
  let acc antennas (pos, c) = CharMap.add_to_list c pos antennas in
  Matrix.mapi add_coord m |> Matrix.fold acc CharMap.empty |> CharMap.remove '.'

let close_antinodes (_ : char Matrix.t) ((r1, c1) : Coord.t)
    ((r2, c2) : Coord.t) : Coord.t list =
  let dr = r2 - r1 in
  let dc = c2 - c1 in
  [ (r1 - dr, c1 - dc); (r2 + dr, c2 + dc) ]

let all_antinodes antinode_fn (m : char Matrix.t) : Coord.Set.t =
  let compute_antinodes (antennas : Coord.t list) : Coord.Set.t =
    List.combinations antennas
    ||> Pair.apply (antinode_fn m)
    ||> Coord.Set.of_list
    |> List.fold_left Coord.Set.union Coord.Set.empty
    |> Coord.Set.filter (Matrix.in_bounds m)
  in
  let acc _ antennas antinodes =
    compute_antinodes antennas |> Coord.Set.union antinodes
  in
  let antennas = find_antennas m in
  CharMap.fold acc antennas Coord.Set.empty

let part_one = all_antinodes close_antinodes >> Coord.Set.cardinal

(*
 * Part 2
 *)
let far_antinodes (m : char Matrix.t) ((r1, c1) : Coord.t) ((r2, c2) : Coord.t)
    : Coord.t list =
  let rec step ((r, c) : Coord.t) (dr : int) (dc : int) : Coord.t list =
    let pos = (r + dr, c + dc) in
    if Matrix.in_bounds m pos then pos :: step pos dr dc else []
  in
  let dr = r2 - r1 in
  let dc = c2 - c1 in
  ((r1, c1) :: step (r1, c1) dr dc) @ step (r1, c1) (-dr) (-dc)

let part_two = all_antinodes far_antinodes >> Coord.Set.cardinal

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
