open Utils

(*
 * Parse input
 *)
let parse (s : string) : string * string =
  let [ a; b ] = String.split_on_char ')' s in
  (a, b)

let puzzle_input = Io.read_lines "2019/data/06.txt" ||> parse

(*
 * Part 1
 *)
module SMap = Map.Make (String)

let to_edge_map (edges : (string * string) list) : string list SMap.t =
  let acc m (l, r) = SMap.add_to_list l r m in
  List.fold_left acc SMap.empty edges

let count_orbits (edges : (string * string) list) : int =
  let edges = to_edge_map edges in
  let rec count_orbits_inner depth node =
    match SMap.find_opt node edges with
    | Some orbits ->
        let sub_orbits =
          orbits ||> count_orbits_inner (depth + 1) |> List.sum
        in
        depth + sub_orbits
    | None -> depth
  in
  count_orbits_inner 0 "COM"

let part_one = count_orbits

(*
 * Part 2
 *)
module SSet = Set.Make (String)

let parent_map (edges : (string * string) list) : string SMap.t =
  let acc m (l, r) = SMap.add r l m in
  List.fold_left acc SMap.empty edges

let get_orbits (parents : string SMap.t) (node : string) : string list =
  let rec get_orbits_inner node =
    match SMap.find_opt node parents with
    | Some parent -> parent :: get_orbits_inner parent
    | None -> []
  in
  get_orbits_inner node

let unique_orbits (left : string list) (right : string list) : int =
  let lefts = SSet.of_list left in
  let rights = SSet.of_list right in
  (SSet.union lefts rights |> SSet.cardinal)
  - (SSet.inter lefts rights |> SSet.cardinal)

let part_two input =
  let parents = parent_map input in
  unique_orbits (get_orbits parents "YOU") (get_orbits parents "SAN")

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
