open Utils

(*
 * Types
 *)
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type component = { name : string; edges : string list }
type edge_map = string list StringMap.t
type component_map = string list StringMap.t

(*
 * Parse input
 *)
let parse_component (line : string) : component =
  let [ name; edges ] = String.split_on_char ':' line in
  { name; edges = edges |> String.trim |> String.split_on_char ' ' }

let puzzle_input = Io.read_lines "2023/data/25.txt" ||> parse_component

(*
 * Part 1
 *)
let make_edge_map (components : component list) : edge_map =
  let get_edges (components : component list) : (string * string) list =
    let attach component =
      List.map (Pair.pack component.name) component.edges
    in
    List.concat_map attach components
  in
  let add_edge edges (src, dest) =
    let append e existing =
      match existing with None -> Some [ e ] | Some es -> Some (e :: es)
    in
    edges
    |> StringMap.update src (append dest)
    |> StringMap.update dest (append src)
  in
  components |> get_edges |> List.fold_left add_edge StringMap.empty

let random_edge (edges : edge_map) : string * string =
  let edges =
    StringMap.to_list edges
    |> List.concat_map (fun (k, vs) -> List.map (Pair.pack k) vs)
  in
  let i = Random.int (List.length edges) in
  List.nth edges i

let contract_edge (edges : edge_map) ((a, b) : string * string) : edge_map =
  let rename_edge vs = List.map (fun n -> if n = b then a else n) vs in
  let new_value =
    StringMap.find a edges @ StringMap.find b edges
    |> List.filter (fun n -> n <> a && n <> b)
    |> List.sort_uniq compare
  in
  let edges' = StringMap.add a new_value edges |> StringMap.remove b in
  StringMap.map rename_edge edges'

let merge_components (components : component_map) ((a, b) : string * string) :
    component_map =
  let new_value = StringMap.find a components @ StringMap.find b components in
  components |> StringMap.remove b |> StringMap.add a new_value

(* Randomly contracts the given graph into two components. *)
let contract_graph (edges : edge_map) : component_map =
  let components = StringMap.mapi (fun k _ -> [ k ]) edges in
  let rec contract_graph' es cs =
    if StringMap.cardinal cs = 2 then cs
    else
      let edge = random_edge es in
      let es' = contract_edge es edge in
      let cs' = merge_components cs edge in
      contract_graph' es' cs'
  in
  contract_graph' edges components

(* Counts how many edges travel between the two components. *)
let count_edges (components : component_map) (edges : edge_map) : int =
  let [ (_, left); (_, right) ] = StringMap.to_list components in
  let left = StringSet.of_list left in
  let dests = List.concat_map (fun n -> StringMap.find n edges) right in
  List.count (fun d -> StringSet.mem d left) dests

(* Finds a cut with n edges that breaks the given graph into two components *)
let find_n_cut ?(n : int = 3) (components : component list) : component_map =
  let edges = make_edge_map components in
  let rec find_n_cut' () =
    let cs = contract_graph edges in
    let count = count_edges cs edges in
    if count = n then cs else find_n_cut' ()
  in
  find_n_cut' ()

let part_one (input : component list) : int =
  let cs = find_n_cut input in
  StringMap.to_list cs ||> snd ||> List.length |> List.product

(*
 * Part 2
 *)
let part_two = Runner.unimplemented

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
