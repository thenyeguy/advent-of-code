open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/03.txt"

(*
 * Part 1
 *)
let to_digits = String.explode >> List.map Char.to_digit

let max_joltage (batteries : int) (bank : int list) : int =
  let rec max_joltage' sum n ds =
    if n = 1 then sum + List.max ds
    else
      let hd = (List.rev >> List.drop (n - 1) >> List.rev) ds in
      let i, d = List.maxi hd in
      max_joltage' (10 * (sum + d)) (n - 1) (List.drop (i + 1) ds)
  in
  max_joltage' 0 batteries bank

let part_one = List.map (to_digits >> max_joltage 2) >> List.sum

(*
 * Part 2
 *)
let part_two = List.map (to_digits >> max_joltage 12) >> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
