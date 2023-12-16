open Utils
open Utils.List.Infix

(*
 * Types
 *)
type lense = { label : string; focal_length : int }
type operation = Pop of string | Push of lense

(*
 * Parse input
 *)
let parse_operation (s : string) : operation =
  match Scanf.sscanf_opt s "%[a-z]-" (fun label -> Pop label) with
  | Some step -> step
  | None ->
      Scanf.sscanf s "%[a-z]=%d" (fun label focal_length ->
          Push { label; focal_length })

let puzzle_input = Io.read_file "data/15.txt" |> String.split_on_char ','

(*
 * Part 1
 *)
let hash (s : string) : int =
  let accumulate acc c = (acc + Char.code c) * 17 mod 256 in
  String.fold_left accumulate 0 s

let part_one (input : string list) = input ||> hash |> List.sum

(*
 * Part 2
 *)
let arrange_lenses (steps : string list) : lense list list =
  let boxes = Array.init 256 (Fn.const []) in
  let remove label box = List.filter (fun l -> l.label <> label) box in
  let update lense box =
    if List.exists (fun lense' -> lense'.label = lense.label) box then
      box ||> fun lense' -> if lense'.label = lense.label then lense else lense'
    else lense :: box
  in
  let execute step =
    match parse_operation step with
    | Pop label ->
        let idx = hash label in
        boxes.(idx) <- remove label boxes.(idx)
    | Push lense ->
        let idx = hash lense.label in
        boxes.(idx) <- update lense boxes.(idx)
  in
  List.iter execute steps;
  boxes |> Array.to_list ||> List.rev

let compute_focusing_power (boxes : lense list list) : int =
  let box_power boxi ls =
    ls
    |> List.mapi (fun i lense -> (i + 1) * lense.focal_length)
    |> List.sum
    |> ( * ) (boxi + 1)
  in
  List.mapi box_power boxes |> List.sum

let part_two (input : string list) : int =
  input |> arrange_lenses |> compute_focusing_power

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
