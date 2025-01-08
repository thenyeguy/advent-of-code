open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/07.txt"

(*
 * Part 1
 *)
let run_network (prog : Intcode.Computer.program_t) (phases : int list) : int =
  let run_amp input phase =
    Intcode.Computer.(init prog [ phase; input ] |> run |> pop |> Option.get)
  in
  List.fold_left run_amp 0 phases

let rec permutations (xs : 'a list) : 'a list list =
  match xs with
  | [] -> []
  | [ x ] -> [ [ x ] ]
  | _ ->
      let permute x =
        List.filter (( <> ) x) xs |> permutations |> List.map (List.cons x)
      in
      List.concat_map permute xs

let part_one program =
  List.range 5 |> permutations ||> run_network program |> List.max

(*
 * Part 2
 *)
let run_recurrent_network (prog : Intcode.Computer.program_t)
    (phases : int list) : int =
  let open Intcode.Computer in
  let cs = phases ||> fun phase -> init prog [ phase ] in
  let run_amp (input : int) (c : t) =
    push c input;
    ignore (run c);
    pop c |> Option.get
  in
  let rec tick (input : int) : int =
    let output = List.fold_left run_amp input cs in
    if (List.hd cs).state = Done then output else tick output
  in
  tick 0

let part_two program =
  List.range ~from:5 10 |> permutations
  ||> run_recurrent_network program
  |> List.max

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
