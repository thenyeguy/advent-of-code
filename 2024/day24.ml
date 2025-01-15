open Utils

(*
 * Parse input
 *)
type op_t = AND | OR | XOR
type instruction_t = string * op_t * string * string

type gates_t = {
  values : (string * bool) list;
  instructions : instruction_t list;
}

let parse (lines : string list) : gates_t =
  let parse_val line =
    Scanf.sscanf line "%[a-z0-9]: %d" (fun g v -> (g, v = 1))
  in
  let parse_op s = match s with "AND" -> AND | "OR" -> OR | "XOR" -> XOR in
  let parse_instr line =
    Scanf.sscanf line "%s %s %s -> %s" (fun l op r o -> (l, parse_op op, r, o))
  in
  let [ values; instructions ] = Io.split_blocks lines in
  {
    values = List.map parse_val values;
    instructions = List.map parse_instr instructions;
  }

let puzzle_input = Io.read_lines "2024/data/24.txt" |> parse

(*
 * Part 1
 *)
module StringReverseAlphabetical = struct
  type t = string

  let compare l r = -compare l r
end

module StringSet = Set.Make (StringReverseAlphabetical)
module StringMap = Map.Make (StringReverseAlphabetical)

let xor (a : bool) (b : bool) : bool = (a && not b) || (b && not a)
let eval (op : op_t) = match op with AND -> ( && ) | OR -> ( || ) | XOR -> xor

(* Returns an updated gates where instructions are sorted by their dependencies. *)
let topological_sort (gates : gates_t) : gates_t =
  let rec pop_instruction (seen : StringSet.t) (instrs : instruction_t list) :
      instruction_t * instruction_t list =
    let is_seen s = StringSet.find_opt s seen |> Option.is_some in
    match instrs with
    | ((l, _, r, _) as i) :: is ->
        if is_seen l && is_seen r then (i, List.tl instrs)
        else
          let i', is' = pop_instruction seen is in
          (i', i :: is')
    | _ -> raise (Failure "no instruction")
  in
  let rec tsort (seen : StringSet.t) (instrs : instruction_t list) :
      instruction_t list =
    if List.is_empty instrs then []
    else
      let ((_, _, _, o) as i), is = pop_instruction seen instrs in
      i :: tsort (StringSet.add o seen) is
  in
  let initials = gates.values ||> fst |> StringSet.of_list in
  let instructions = tsort initials gates.instructions in
  { values = gates.values; instructions }

let simulate (gates : gates_t) : bool StringMap.t =
  let apply (values : bool StringMap.t) ((left, op, right, out) : instruction_t)
      : bool StringMap.t =
    let l = StringMap.find left values in
    let r = StringMap.find right values in
    let o = eval op l r in
    StringMap.add out o values
  in
  let initial_values = StringMap.of_list gates.values in
  List.fold_left apply initial_values gates.instructions

let read_output (values : bool StringMap.t) : int =
  (* We assume the map is sorted reverse alphabetically, so that higher bits come first.
   * This is implemented by the map compoarator. *)
  let acc key value total =
    if String.starts_with ~prefix:"z" key then
      let bit = if value then 1 else 0 in
      (2 * total) + bit
    else total
  in
  StringMap.fold acc values 0

let part_one = topological_sort >> simulate >> read_output

(*
 * Part 2
 *)
let edge_map (gates : gates_t) : instruction_t list StringMap.t =
  let acc m ((l, _, r, _) as i) =
    m |> StringMap.add_to_list l i |> StringMap.add_to_list r i
  in
  List.fold_left acc StringMap.empty gates.instructions

(* Convenience for debugging: *)
let edges n = edge_map puzzle_input |> StringMap.find n

let check_adders (gates : gates_t) =
  (* Get graph edges: *)
  let edges = edge_map gates in
  (* Conveniences: *)
  let is_and (_, op, _, _) = op = AND in
  let is_or (_, op, _, _) = op = OR in
  let is_xor (_, op, _, _) = op = XOR in
  let has_input (l, _, r, _) i = l = i || r = i in
  (* Validate a single adder is connected correctly:
   *
   * w[n] = x[n] XOR y[n]
   * b[n] = x[n] AND y[n]
   *
   * z[n] = w[n] XOR c[n-1]
   *
   * e[n] = w[n] AND c[n-1]
   * c[n] = b[n] OR e[n]
   *
   * Bad output gates are returned.
   *)
  let check_adder (cn1 : string) (n : int) : string * string list =
    let xn = Format.sprintf "x%02d" n in
    let zn = Format.sprintf "z%02d" n in
    let _, _, _, wn = StringMap.find xn edges |> List.find is_xor in
    let _, _, _, bn = StringMap.find xn edges |> List.find is_and in
    let en_instr = StringMap.find wn edges |> List.find_opt is_and in
    let zn_instr = StringMap.find wn edges |> List.find_opt is_xor in
    let cn_instr =
      try StringMap.find bn edges |> List.find_opt is_or
      with Not_found -> None
    in
    let wn_check =
      if Option.is_none en_instr || Option.is_none zn_instr then [ wn ] else []
    in
    let bn_check = if Option.is_none cn_instr then [ bn ] else [] in
    let zn_check =
      match zn_instr with
      | Some (_, _, _, zn') when zn <> zn' -> [ zn'; zn ]
      | _ -> []
    in
    let cn1_check =
      match zn_instr with
      | Some instr when not (has_input instr cn1) -> [ cn1 ]
      | _ -> []
    in
    let cn = match cn_instr with Some (_, _, _, cn) -> cn | _ -> "" in
    (cn, wn_check @ bn_check @ zn_check @ cn1_check)
  in
  (* Check every adder. *)
  let _, _, _, c0 = StringMap.find "x00" edges |> List.find is_and in
  List.range ~from:1 45
  |> List.fold_left_map check_adder c0
  |> snd |> List.flatten |> List.sort_uniq compare
  |> List.filter (( <> ) "")

let part_two = check_adders >> String.concat ","

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input (part_one >> string_of_int) part_two
