open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/06.txt"

(*
 * Part 1
 *)
type equation = { args : int list; op : char }

let parse (lines : string list) : equation list =
  let rec group_equations args ops =
    match ops with
    | [] -> []
    | op :: ops' ->
        let args' = List.map List.tl args in
        let args = List.map List.hd args in
        { op; args } :: group_equations args' ops'
  in
  let (ops :: args) = List.rev lines in
  let args = List.map List.of_string args in
  let ops = String.explode ops |> List.filter (( != ) ' ') in
  group_equations args ops

let eval (eq : equation) : int =
  match eq.op with
  | '+' -> List.fold_left ( + ) 0 eq.args
  | '*' -> List.fold_left ( * ) 1 eq.args

let part_one = parse >> List.map eval >> List.sum

(*
 * Part 2
 *)
let parse_columnwise (lines : string list) : equation list =
  let rec read_args acc lines =
    if (List.hd >> List.is_empty) lines then [ acc ]
    else
      let hd = List.map List.hd lines in
      let lines' = List.map List.tl lines in
      let s = String.implode hd in
      match (String.trim >> int_of_string_opt) s with
      | Some i -> read_args (i :: acc) lines'
      | None -> acc :: read_args [] lines'
  in
  let group_equations args ops =
    let make_eq (args, op) = { args; op } in
    List.combine args ops ||> make_eq
  in
  let (ops :: args) = List.rev lines in
  let ops = String.explode ops |> List.filter (( != ) ' ') in
  let args = (List.rev >> List.map String.explode >> read_args []) args in
  group_equations args ops

let part_two = parse_columnwise >> List.map eval >> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
