open Utils

(*
 * Parse input
 *)
let parse_line line =
  let [ l; _; _; r ] = String.split_on_char ' ' line in
  (int_of_string l, int_of_string r)

let puzzle_input = Io.read_lines "2024/data/01.txt" ||> parse_line |> List.split

(*
 * Part 1
 *)
let distances ls rs =
  let distance l r = Int.abs (l - r) in
  List.map2 distance (List.sort compare ls) (List.sort compare rs)

let similarity ls rs =
  let count l = List.count (( = ) l) rs * l in
  ls ||> count |> List.sum

let part_one input = input |> Pair.apply distances |> List.sum

(*
 * Part 2
 *)
let part_two input = input |> Pair.apply similarity

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
