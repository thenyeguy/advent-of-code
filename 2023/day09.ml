open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2023/data/09.txt" ||> List.of_string

(*
 * Part 1
 *)
let derivative = List.pair_map ( - )
let all_zeros = List.all (( = ) 0)

let rec next_step (values : int list) : int =
  let deltas = derivative values in
  if all_zeros deltas then List.hd values
  else List.last values + next_step deltas

let part_one (input : int list list) = input ||> next_step |> List.sum

(*
 * Part 2
 *)
let part_two (input : int list list) =
  input ||> List.rev ||> next_step |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
