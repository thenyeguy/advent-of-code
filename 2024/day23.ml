open Utils

(*
 * Parse input
 *)
type edge_t = string * string

let parse_edge (line : string) : edge_t =
  let [ l; r ] = String.split_on_char '-' line in
  (l, r)

let puzzle_input = Io.read_lines "2024/data/23.txt" ||> parse_edge

(*
 * Part 1
 *)
module SSet = Set.Make (String)
module SMap = Map.Make (String)

module TripleSet = Set.Make (struct
  type t = string * string * string

  let compare = compare
end)

type edge_map = SSet.t SMap.t

let edge_set (edges : edge_t list) : edge_map =
  let add_edge m (l, r) = m |> SMap.add_to_list l r |> SMap.add_to_list r l in
  List.fold_left add_edge SMap.empty edges |> SMap.map SSet.of_list

let find_triples (edges : edge_map) : TripleSet.t =
  let find_triples_inner node outgoing triples =
    let inter second = (second, SSet.inter outgoing (SMap.find second edges)) in
    let make_triples (second, thirds) =
      SSet.to_list thirds ||> fun third -> (node, second, third)
    in
    let is_sorted (a, b, c) = a < b && b < c in
    SSet.to_list outgoing ||> inter
    |> List.concat_map make_triples
    |> List.filter is_sorted
    |> List.fold_left (Fn.swap TripleSet.add) triples
  in
  SMap.fold find_triples_inner edges TripleSet.empty

let is_t_triple (a, b, c) : bool =
  let swith = String.starts_with ~prefix:"t" in
  swith a || swith b || swith c

let part_one input =
  input |> edge_set |> find_triples
  |> TripleSet.filter is_t_triple
  |> TripleSet.cardinal

(*
 * Part 2
 *)
let biggest_set (sets : SSet.t list) : SSet.t =
  let acc biggest s =
    if SSet.cardinal s > SSet.cardinal biggest then s else biggest
  in
  List.fold_left acc SSet.empty sets

let largest_component (edges : edge_map) : SSet.t =
  let keys = SMap.bindings edges ||> Pair.left |> SSet.of_list in
  let rec expand_component (nodes : SSet.t) : SSet.t =
    let new_edges n = SMap.find n edges |> Fn.swap SSet.diff nodes in
    let candidates =
      SSet.to_list nodes ||> new_edges |> List.fold_left SSet.inter keys
    in
    match SSet.min_elt_opt candidates with
    | Some e -> SSet.add e nodes |> expand_component
    | None -> nodes
  in
  SSet.to_list keys ||> SSet.singleton ||> expand_component |> biggest_set

let string_of_set (set : SSet.t) : string =
  SSet.to_list set |> String.concat ","

let part_two = edge_set >> largest_component >> string_of_set

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input (part_one >> string_of_int) part_two
