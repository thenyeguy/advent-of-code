open Utils

(*
 * Types
 *)
module StringMap = Map.Make (String)

(* Parts *)
type attr = X | M | A | S
type part = { x : int; m : int; a : int; s : int }

(* Part ranges *)
type range = int * int (* [lower, upper) *)
type part_range = { xs : range; ms : range; as_ : range; ss : range }

(* Rules *)
type result = Run of string | Accept | Reject
type op = Less of attr * int | Greater of attr * int | Always
type rule = op * result

(* Full input *)
type workflow = string * rule list
type input = workflow list * part list

(*
 * Parse input
 *)
let parse_attr (c : char) : attr =
  match c with 'x' -> X | 'm' -> M | 'a' -> A | 's' -> S

let parse_rule (s : string) : rule =
  let parse_target s =
    match s with "A" -> Accept | "R" -> Reject | _ -> Run s
  in
  if String.contains s ':' then
    Scanf.sscanf s "%c%c%d:%s" (fun attr op value target ->
        if op = '<' then (Less (parse_attr attr, value), parse_target target)
        else (Greater (parse_attr attr, value), parse_target target))
  else (Always, parse_target s)

let parse_workflow (line : string) : workflow =
  Scanf.sscanf line "%[a-z]{%[^}]}" (fun label rules ->
      let rules = String.split_on_char ',' rules ||> parse_rule in
      (label, rules))

let parse_part (line : string) : part =
  Scanf.sscanf line "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })

let parse_input (lines : string list) : input =
  let [ workflows; parts ] = Io.split_blocks lines in
  (List.map parse_workflow workflows, List.map parse_part parts)

let puzzle_input = Io.read_lines "2023/data/19.txt" |> parse_input

(*
 * Part 1
 *)
let run_rule ((op, result) : rule) (part : part) : result option =
  let get_attr (a : attr) (p : part) : int =
    match a with X -> p.x | M -> p.m | A -> p.a | S -> p.s
  in
  let apply op attr n =
    if op (get_attr attr part) n then Some result else None
  in
  match op with
  | Less (attr, n) -> apply ( < ) attr n
  | Greater (attr, n) -> apply ( > ) attr n
  | Always -> Some result

let run_workflows (workflows : rule list StringMap.t) (part : part) : bool =
  let rec run_workflow label =
    let rules = StringMap.find label workflows in
    let apply acc rule = Option.or_ acc (run_rule rule part) in
    match List.fold_left apply None rules with
    | Some Accept -> true
    | Some Reject -> false
    | Some (Run w) -> run_workflow w
  in
  run_workflow "in"

let score_part (p : part) : int = p.x + p.m + p.a + p.s

let part_one ((workflows, parts) : input) : int =
  let workflows = StringMap.of_list workflows in
  parts |> List.filter (run_workflows workflows) ||> score_part |> List.sum

(*
 * Part 2
 *)
let range_size (parts : part_range) =
  let size (lower, upper) = upper - lower + 1 in
  size parts.xs * size parts.ms * size parts.as_ * size parts.ss

let split_range (op : op) (range : range) : range option * range option =
  let lower, upper = range in
  match op with
  | Less (_, n) ->
      if upper < n then (Some range, None)
      else if lower >= n then (None, Some range)
      else (Some (lower, n - 1), Some (n, upper))
  | Greater (_, n) ->
      if upper <= n then (None, Some range)
      else if lower > n then (Some range, None)
      else (Some (n + 1, upper), Some (lower, n))

let split_part (op : op) (part : part_range) :
    part_range option * part_range option =
  let attr =
    match op with Less (attr, _) -> attr | Greater (attr, _) -> attr
  in
  let range =
    match attr with X -> part.xs | M -> part.ms | A -> part.as_ | S -> part.ss
  in
  let update range' =
    match attr with
    | X -> { xs = range'; ms = part.ms; as_ = part.as_; ss = part.ss }
    | M -> { xs = part.xs; ms = range'; as_ = part.as_; ss = part.ss }
    | A -> { xs = part.xs; ms = part.ms; as_ = range'; ss = part.ss }
    | S -> { xs = part.xs; ms = part.ms; as_ = part.as_; ss = range' }
  in
  let left, right = split_range op range in
  Option.(map update left, map update right)

let count_valid_parts (workflows : rule list StringMap.t) : int =
  let rec run_workflow (label : string) (range : part_range) : int =
    let run_rule range' rule =
      match range' with
      | None -> (None, 0)
      | Some range' ->
          let op, result = rule in
          let left, right =
            match op with
            | Always -> (Some range', None)
            | _ -> split_part op range'
          in
          let count =
            match left with
            | None -> 1
            | Some left -> (
                match result with
                | Accept -> range_size left
                | Reject -> 0
                | Run s -> run_workflow s left)
          in
          (right, count)
    in
    StringMap.find label workflows
    |> List.fold_left_map run_rule (Some range)
    |> snd |> List.sum
  in
  run_workflow "in"
    { xs = (1, 4000); ms = (1, 4000); as_ = (1, 4000); ss = (1, 4000) }

let part_two ((workflows, _) : input) : int =
  count_valid_parts (StringMap.of_list workflows)

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
