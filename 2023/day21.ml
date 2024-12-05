open Utils
open Utils.List.Infix

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2023/data/21.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let get_adjacencies (garden : char Matrix.t) (c : Coord.t) : Coord.t list =
  let is_open c' = Matrix.get garden c' <> '#' in
  Coord.[ step Up c; step Left c; step Right c; step Down c ]
  |> List.filter (Matrix.in_bounds garden)
  |> List.filter is_open

let steps_from (garden : char Matrix.t) (c : Coord.t) : (Coord.t * int) list =
  let rec steps_from' (counts : int Coord.Map.t) (frontier : Coord.t list)
      (steps : int) =
    if List.is_empty frontier then counts
    else
      let counts' =
        Coord.Map.add_seq (frontier ||> Pair.rpack steps |> List.to_seq) counts
      in
      let frontier' =
        frontier
        |> List.concat_map (get_adjacencies garden)
        |> List.filter (fun k -> not (Coord.Map.mem k counts))
        |> List.sort_uniq compare
      in
      steps_from' counts' frontier' (steps + 1)
  in
  steps_from' Coord.Map.empty [ c ] 0 |> Coord.Map.to_list

let reachable_from (garden : char Matrix.t) (c : Coord.t) (steps : int) : int =
  steps_from garden c
  |> List.count (fun (_, n) -> n <= steps && n mod 2 = steps mod 2)

let part_one (input : char Matrix.t) : int =
  let (Some start) = Matrix.find (( = ) 'S') input in
  reachable_from input start 64

(*
 * Part 2
 *
 * We rely on the garden being square, with empty borders and center rows, and
 * diagonals. This means that traveling out from the center, edges and corners
 * tiles cleanly.
 *
 * Due to the garden size being odd, we will alternate parities of the different
 * tiled gardens. The center is odd parity:
 *    e o e
 *  e o E o e
 *  o E O E o
 *  e o E o e
 *    e o e
 *
 * Finally, 26501365/131 = 202300 rem 65, which means in a straight line
 * we travel exactly to the edge of the outermost tiles. This cuts off the
 * corners of the odd polarities on the outside, and add corners of even
 * polarities, so after counting the total number of traveresed tiles, we have
 * to add/subtract the corners back in.
 *)
let reachable_plots (steps : int) (garden : char Matrix.t) =
  let rows, cols = Matrix.size garden in
  let steps_from' c = steps_from garden c ||> Pair.right in
  let odds f = List.count (fun n -> n mod 2 = 1 && f n) in
  let evens f = List.count (fun n -> n mod 2 = 0 && f n) in
  let all = steps_from' (rows / 2, cols / 2) in
  let all_odds = odds (Fn.const true) all in
  let all_evens = evens (Fn.const true) all in
  let from_corners =
    [
      steps_from' (0, 0);
      steps_from' (0, cols - 1);
      steps_from' (rows - 1, 0);
      steps_from' (cols - 1, cols - 1);
    ]
  in
  let even_corners = from_corners ||> evens (Fn.lt (rows / 2)) |> List.sum in
  let odd_corners = odds (Fn.gt (rows / 2)) all in
  let n =
    (steps - (rows / 2)) / rows
    (* tiles traveled in one direction *)
  in
  ((n + 1) * (n + 1) * all_odds)
  + (n * n * all_evens)
  - ((n + 1) * odd_corners)
  + (n * even_corners)

let part_two (input : char Matrix.t) : int = reachable_plots 26501365 input

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
