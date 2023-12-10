open Utils
open Utils.Infix
module StringMap = Map.Make (String)

(*
 * Parse input
 *)
type direction = Left | Right
type node = { name : string; left : string; right : string }
type map = { directions : direction Seq.t; nodes : node list }

let node_map (nodes : node list) : node StringMap.t =
  nodes ||> (fun n -> (n.name, n)) |> StringMap.of_list

let parse_input (input : string list) : map =
  let parse_direction c = match c with 'L' -> Left | 'R' -> Right in
  let make_node n l r = { name = n; left = l; right = r } in
  let parse_node node =
    Scanf.sscanf node "%[A-Z] = (%[A-Z], %[A-Z])" make_node
  in
  let [ [ head ]; tail ] = Misc.split_blocks input in
  let directions =
    head |> String.explode ||> parse_direction |> List.to_seq |> Seq.cycle
  in
  let nodes = tail ||> parse_node in
  { directions; nodes }

let puzzle_input () = Io.read_lines "data/08.txt" |> parse_input

(*
 * Part 1
 *)
let next_node (node_map : node StringMap.t) (dir : direction) (node : string) :
    string =
  let node = StringMap.find node node_map in
  match dir with Left -> node.left | Right -> node.right

let count_steps (m : map) (is_end_node : string -> bool)
    (starting_node : string) : int =
  let node_map = node_map m.nodes in
  let rec _count_steps node dirs steps =
    if is_end_node node then steps
    else
      let (Option.Some (dir, dirs)) = Seq.uncons dirs in
      _count_steps (next_node node_map dir node) dirs (steps + 1)
  in
  _count_steps starting_node m.directions 0

let part_one (input : map) = count_steps input (( = ) "ZZZ") "AAA"

(*
 * Part 2
 *)
let get_starting_nodes (m : map) : string list =
  m.nodes ||> (fun n -> n.name) |> List.filter (String.ends_with ~suffix:"A")

let reached_ending_nodes (nodes : string list) : bool =
  nodes ||> String.ends_with ~suffix:"Z" |> List.all

let lcm a b =
  let rec gcd n =
    match (a mod n, b mod n) with 0, 0 -> n | _ -> gcd (n - 1)
  in
  a * b / gcd (min a b)

let count_ghost_steps (m : map) =
  (* We assume there are unique cycles for each path; then check for the
     combined periodicity of all the cycles *)
  let starting_nodes = get_starting_nodes m in
  let steps = starting_nodes ||> count_steps m (String.ends_with ~suffix:"Z") in
  List.fold_left lcm 1 steps

let part_two = count_ghost_steps

(*
 * Main
 *)
let _ = Runner.main (puzzle_input ()) part_one part_two
