open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/01.txt"

(*
 * Part 1
 *)
let parse (s : string) : int =
  let (hd :: tl) = String.explode s in
  let sign = match hd with 'R' -> 1 | 'L' -> -1 in
  let dist = String.implode tl |> int_of_string in
  sign * dist

let spin_dial (count : int -> int -> int) (rs : int list) : int =
  let rec spin hits n rs =
    match rs with
    | [] -> hits
    | r :: rs' ->
        let n' = n + r in
        (spin [@tailcall]) (hits + count n n') (Math.pos_mod n' 100) rs'
  in
  spin 0 50 rs

let part_one input =
  let count _ n' = if n' mod 100 = 0 then 1 else 0 in
  input ||> parse |> spin_dial count

(*
 * Part 2
 *)
let part_two input =
  let count n n' =
    let hits = abs (n' / 100) in
    if n' <= 0 && n > 0 then hits + 1 else hits
  in
  input ||> parse |> spin_dial count

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
