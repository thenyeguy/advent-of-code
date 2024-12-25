open Utils
open Utils.Fn.Infix
open Utils.List.Infix

(*
 * Parse input
 *)
type input_t = char Matrix.t list

let puzzle_input : input_t =
  Io.read_lines "2024/data/25.txt" |> Io.split_blocks ||> Matrix.of_strings

(*
 * Part 1
 *)
type schematics_t = { locks : int list list; keys : int list list }

let count_pins (m : char Matrix.t) : int list =
  let count (a : char array) : int =
    Array.to_list a |> List.count (( = ) '#') |> Fn.sub 1
  in
  Matrix.transpose m |> Array.map count |> Array.to_list

let rec read_schematic (input : input_t) : schematics_t =
  match input with
  | i :: is ->
      let pins = count_pins i in
      let schematics = read_schematic is in
      if Matrix.get i (0, 0) = '#' then
        { locks = pins :: schematics.locks; keys = schematics.keys }
      else { locks = schematics.locks; keys = pins :: schematics.keys }
  | [] -> { locks = []; keys = [] }

let rec fits_lock (key : int list) (lock : int list) : bool =
  match (key, lock) with
  | k :: ks, l :: ls -> k + l <= 5 && fits_lock ks ls
  | [], [] -> true

let count_fits (schematic : schematics_t) : int =
  let count_locks key = List.count (fits_lock key) schematic.locks in
  schematic.keys ||> count_locks |> List.sum

let part_one = read_schematic >> count_fits

(*
 * Part 2
 *)
let part_two = Runner.unimplemented

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
