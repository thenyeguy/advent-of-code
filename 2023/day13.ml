open Utils
open Utils.List.Infix

(*
 * Parse input
 *)
let puzzle_input =
  Io.read_lines "2023/data/13.txt" |> Io.split_blocks ||> Matrix.of_strings

(*
 * Part 1
 *)
let find_row_reflection ?(smudges = 0) (pattern : char Matrix.t) : int option =
  let count_smudges left right =
    let count_line_smudges left right =
      let count_smudge l r = if l = r then 0 else 1 in
      Array.map2 count_smudge left right |> Array.fold_left ( + ) 0
    in
    let left, right = (List.to_seq left, List.to_seq right) in
    Seq.map2 count_line_smudges left right |> Seq.fold_left ( + ) 0
  in
  let rec find_row_reflection' left right count =
    if count_smudges left right = smudges then Option.some count
    else
      match right with
      | [] -> Option.none
      | [ _ ] -> Option.none
      | r :: right' -> find_row_reflection' (r :: left) right' (count + 1)
  in
  let (first :: rest) = Array.to_list pattern in
  find_row_reflection' [ first ] rest 1

let find_col_reflection ?smudges (pattern : char Matrix.t) : int option =
  pattern |> Matrix.transpose |> find_row_reflection ?smudges

let count_reflections ?smudges (patterns : char Matrix.t list) : int =
  let rows = patterns |> List.filter_map (find_row_reflection ?smudges) in
  let cols = patterns |> List.filter_map (find_col_reflection ?smudges) in
  (100 * List.sum rows) + List.sum cols

let part_one (patterns : char Matrix.t list) : int = count_reflections patterns

(*
 * Part 2
 *)
let part_two (patterns : char Matrix.t list) : int =
  count_reflections ~smudges:1 patterns

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
