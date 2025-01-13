open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/02.txt" ||> List.of_string

(*
 * Part 1
 *)
let rec diffs ls =
  match ls with l :: r :: _ -> (r - l) :: diffs (List.tl ls) | _ -> []

let is_same_sign ds =
  let (d :: ds) = ds in
  let same_sign' = if d < 0 then Fn.lt 0 else Fn.gt 0 in
  ds ||> same_sign' |> List.all

let is_small ds =
  let is_small' d =
    let d' = Int.abs d in
    1 <= d' && d' <= 3
  in
  ds ||> is_small' |> List.all

let is_safe ls =
  let ds = diffs ls in
  is_same_sign ds && is_small ds

let part_one input = input |> List.count is_safe

(*
 * Part 2
 *)

let safe_after_removal ls =
  let (d :: ds) = diffs ls in
  let same_sign = if d < 0 then Fn.lt 0 else Fn.gt 0 in
  let is_small d' = Int.abs d' <= 3 in
  let is_valid d' = same_sign d' && is_small d' in
  let is_safe_tail ds' = ds' ||> is_valid |> List.all in
  (* Search for the first unsafe diff, then validate the tail after removal. *)
  let rec is_safe' ds' =
    match ds' with
    | [] -> true
    | [ _ ] -> true
    | d :: ds'' when is_valid d -> is_safe' ds''
    | d1 :: d2 :: ds'' when is_valid (d1 + d2) -> is_safe_tail ds''
    | _ -> false
  in
  (* We must manually check if the tail is valid, as we don't know if it is the
   * correct sign *)
  is_safe' (d :: ds) || is_safe (List.tl ls)

let part_two input = List.count safe_after_removal input

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
