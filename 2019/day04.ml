open Utils

(*
 * Parse input
 *)
let parse (s : string) : int * int =
  let [ lower; upper ] = String.split_on_char '-' s in
  (int_of_string lower, int_of_string upper)

let puzzle_input = Io.read_file "2019/data/04.txt" |> parse

(*
 * Part 1
 *)
let rec digits (n : int) : int list =
  if n = 0 then [] else digits (n / 10) @ [ n mod 10 ]

let rec is_increasing (floor : int) (digits : int list) : bool =
  match digits with d :: ds' -> d >= floor && is_increasing d ds' | [] -> true

let rec has_double (digits : int list) : bool =
  match digits with
  | a :: b :: _ when a = b -> true
  | _ :: tl -> has_double tl
  | [] -> false

let validate_one (n : int) : bool =
  let ds = digits n in
  is_increasing 0 ds && has_double ds

let part_one (lower, upper) =
  Seq.range ~from:lower (upper + 1) |> Seq.count validate_one

(*
 * Part 2
 *)
let has_exact_double (digits : int list) : bool =
  List.group ( = ) digits ||> List.length |> List.exists (( = ) 2)

let validate_two (n : int) : bool =
  let ds = digits n in
  is_increasing 0 ds && has_exact_double ds

let part_two (lower, upper) =
  Seq.range ~from:lower (upper + 1) |> Seq.count validate_two

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
