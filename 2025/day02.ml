open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_file "2025/data/02.txt"

(*
 * Part 1
 *)
type range = int * int

let parse (s : string) : range list =
  let parse_range (s : string) : range =
    let [ from; to_ ] = String.split_on_char '-' s in
    (int_of_string from, int_of_string to_)
  in
  String.split_on_char ',' s ||> parse_range

let is_doubled (id : int) : bool =
  let digits = (float_of_int >> log10 >> int_of_float) id + 1 in
  let div = (float_of_int >> Float.pow 10. >> int_of_float) (digits / 2) in
  id / div = id mod div

let sum_invalids (is_invalid : int -> bool) (rs : range list) : int =
  let acc sum (from, to_) =
    sum
    + (Seq.irange ~from to_ |> Seq.filter is_invalid |> Seq.fold_left ( + ) 0)
  in
  List.fold_left acc 0 rs

let part_one = parse >> sum_invalids is_doubled

(*
 * Part 2
 *)
let repeats_on (div : int) (n : int) : bool =
  let hd = n mod div in
  let rec repeats' tl =
    if tl = 0 then true
    else if tl < div / 10 then false
    else if hd != tl mod div then false
    else repeats' (tl / div)
  in
  repeats' (n / div)

let is_repeated (id : int) : bool =
  let rec check div =
    if div > id then false
    else if repeats_on div id then true
    else check (div * 10)
  in
  check 10

let part_two = parse >> sum_invalids is_repeated

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
