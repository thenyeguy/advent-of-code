open Utils

(*
 * Parse input
 *)
type machine = { a : int * int; b : int * int; prize : int * int }

let parse_machine (lines : string list) : machine =
  let [ a; b; prize ] = lines in
  let a = Scanf.sscanf a "Button A: X+%d, Y+%d" (fun x y -> (x, y)) in
  let b = Scanf.sscanf b "Button B: X+%d, Y+%d" (fun x y -> (x, y)) in
  let prize = Scanf.sscanf prize "Prize: X=%d, Y=%d" (fun x y -> (x, y)) in
  { a; b; prize }

let puzzle_input =
  Io.read_lines "2024/data/13.txt" |> Io.split_blocks ||> parse_machine

(*
 * Part 1
 *)
let to_int (f : float) : int option =
  let i = f |> Float.round |> int_of_float in
  if Float.abs (f -. float_of_int i) < 0.01 then Some i else None

let button_presses (m : machine) : (int * int) option =
  let a =
    Matrix.map float_of_int [| [| fst m.a; fst m.b |]; [| snd m.a; snd m.b |] |]
  in
  let b = Array.map float_of_int [| fst m.prize; snd m.prize |] in
  let x = Lin_alg.solve a b in
  match (to_int x.(0), to_int x.(1)) with
  | Some a, Some b when a >= 0 && b >= 0 -> Some (a, b)
  | _ -> None

let is_legal ((a, b) : int * int) : bool = a <= 100 && b <= 100
let price ((a, b) : int * int) : int = (3 * a) + b

let part_one input =
  List.filter_map button_presses input
  |> List.filter is_legal ||> price |> List.sum

(*
 * Part 2
 *)
let fix_machine (m : machine) : machine =
  let fix_prize (x, y) = (x + 10000000000000, y + 10000000000000) in
  { a = m.a; b = m.b; prize = fix_prize m.prize }

let part_two input =
  input ||> fix_machine |> List.filter_map button_presses ||> price |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
