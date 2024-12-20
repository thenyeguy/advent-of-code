open Utils
open Utils.Fn.Infix
open Utils.List.Infix

(*
 * Parse input
 *)
type towels = { patterns : string list; designs : string list }

let parse (lines : string list) : towels =
  let [ [ patterns ]; designs ] = Io.split_blocks lines in
  let patterns = String.split_on_char ',' patterns ||> String.trim in
  { patterns; designs }

let puzzle_input = Io.read_lines "2024/data/19.txt" |> parse

(*
 * Part 1
 *)
let strip_prefix (prefix : string) (s : string) : string option =
  if String.starts_with ~prefix s then
    let start = String.length prefix in
    let len = String.length s - start in
    Some (String.sub s start len)
  else None

let possible_arrangements (patterns : string list) (design : string) : int =
  let is_possible_inner memoed d =
    if d = "" then 1
    else
      let apply_pattern p = strip_prefix p d in
      List.filter_map apply_pattern patterns ||> memoed |> List.sum
  in
  Memo.memo_rec is_possible_inner design

let part_one input =
  List.count (possible_arrangements input.patterns >> Fn.gt 0) input.designs

(*
 * Part 2
 *)
let part_two input =
  input.designs ||> possible_arrangements input.patterns |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
