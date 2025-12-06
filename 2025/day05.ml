open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/05.txt"

(*
 * Part 1
 *)
type range = int * int
type ingredient_db = { fresh_ranges : range list; ingredients : int list }

let parse (lines : string list) : ingredient_db =
  let [ ranges; ids ] = Io.split_blocks lines in
  let parse_range (s : string) : range =
    let [ from; to_ ] = String.split_on_char '-' s in
    (int_of_string from, int_of_string to_)
  in
  {
    fresh_ranges = List.map parse_range ranges;
    ingredients = List.map int_of_string ids;
  }

let is_fresh (ranges : range list) (ingredient : int) : bool =
  let in_range (l, u) = l <= ingredient && ingredient <= u in
  List.count in_range ranges > 0

let part_one input =
  let db = parse input in
  List.count (is_fresh db.fresh_ranges) db.ingredients

(*
 * Part 2
 *)
let combine_ranges (ranges : range list) : range list =
  let rec merge ranges =
    match ranges with
    | (al, au) :: (bl, bu) :: tl when bl <= au -> merge ((al, max au bu) :: tl)
    | hd :: tl -> hd :: merge tl
    | [] -> []
  in
  (List.sort compare >> merge) ranges

let count_ingredients (ranges : range list) : int =
  let acc sum (l, u) = sum + u - l + 1 in
  List.fold_left acc 0 ranges

let part_two input =
  let db = parse input in
  (combine_ranges >> count_ingredients) db.fresh_ranges

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
