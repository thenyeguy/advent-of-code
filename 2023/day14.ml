open Utils
open Utils.List.Infix

(*
 * Types
 *)
type dir = North | West | South | East
type cycle = { start : int; length : int }

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2023/data/14.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let tilt_row (dir : dir) (row : char array) : char array =
  let order l r =
    if dir = North || dir = West then -compare l r else compare l r
  in
  let should_group l r = if l = '#' then r = '#' else r <> '#' in
  List.(
    Array.to_list row |> group should_group ||> sort order |> flatten
    |> Array.of_list)

let tilt (dir : dir) (dish : char Matrix.t) : char Matrix.t =
  if dir = North || dir = South then
    dish |> Matrix.transpose |> Array.map (tilt_row dir) |> Matrix.transpose
  else dish |> Array.map (tilt_row dir)

let compute_load_north (dish : char Matrix.t) : int =
  let add_load weight acc c = acc + if c = 'O' then weight else 0 in
  let compute_load_row weight row = Array.fold_left (add_load weight) 0 row in
  let weights = List.range ~from:(Array.length dish) 0 in
  dish |> Array.to_list |> List.map2 compute_load_row weights |> List.sum

let part_one (input : char Matrix.t) = input |> tilt North |> compute_load_north

(*
 * Part 2
 *)
let run_spin_cycle (dish : char Matrix.t) : char Matrix.t =
  dish |> tilt North |> tilt West |> tilt South |> tilt East

let rec run_spin_cycles (cycles : int) (dish : char Matrix.t) : char Matrix.t =
  if cycles > 0 then run_spin_cycles (cycles - 1) (run_spin_cycle dish)
  else dish

exception FoundCycle of cycle

let find_cycle (dish : char Matrix.t) : cycle =
  let h = Hashtbl.create 100 in
  let dish = ref dish in
  try
    for i = 1 to 1_000_000_000 do
      dish := run_spin_cycle !dish;
      match Hashtbl.find_opt h !dish with
      | Some start -> raise (FoundCycle { start; length = i - start })
      | _ -> Hashtbl.add h !dish i
    done;
    raise (Failure "find cycle")
  with FoundCycle cycle -> cycle

let run_all_spin_cycles (dish : char Matrix.t) =
  let { start; length } = find_cycle dish in
  let remaining = (1_000_000_000 - start) mod length in
  run_spin_cycles (start + remaining) dish

let part_two (input : char Matrix.t) =
  input |> run_all_spin_cycles |> compute_load_north

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
