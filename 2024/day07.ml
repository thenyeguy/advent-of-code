open Utils

(*
 * Parse input
 *)
type equation = { solution : int; args : int list }

let parse_equation (line : string) : equation =
  let [ solution; args ] = String.split_on_char ':' line in
  let args =
    String.split_on_char ' ' args |> List.filter_map int_of_string_opt
  in
  { solution = int_of_string solution; args }

let puzzle_input = Io.read_lines "2024/data/07.txt" ||> parse_equation

(*
 * Part 1
 *)

let will_evaluate (ops : (int -> int -> int) list) (eq : equation) : bool =
  let rec will_evaluate' solution args : bool =
    let apply_op l r op = op l r in
    let recurse tl n = will_evaluate' solution (n :: tl) in
    match args with
    | l :: r :: tl -> ops ||> apply_op l r ||> recurse tl |> List.any
    | [ n ] -> n = solution
  in
  will_evaluate' eq.solution eq.args

let part_one inputs =
  let solution eq = eq.solution in
  List.filter (will_evaluate [ ( + ); ( * ) ]) inputs ||> solution |> List.sum

(*
 * Part 2
 *)
let concat (l : int) (r : int) : int =
  int_of_string (string_of_int l ^ string_of_int r)

let part_two inputs =
  let solution eq = eq.solution in
  List.filter (will_evaluate [ ( + ); ( * ); concat ]) inputs
  ||> solution |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
