open Utils
module IntMap = Map.Make (Int)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/16.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
module Graph = struct
  type t = char matrix
  type node = coord * dir

  let neighbors (g : t) ((pos, dir) : node) : node list =
    let turns = [ (pos, Coord.turn_left dir); (pos, Coord.turn_right dir) ] in
    let step = Coord.step pos dir in
    match Matrix.get_opt g step with
    | Some '.' | Some 'S' | Some 'E' -> (step, dir) :: turns
    | _ -> turns

  let cost (_ : t) ((_, src_dir) : node) ((_, dest_dir) : node) : int =
    if src_dir = dest_dir then 1 else 1000

  let is_done (g : t) ((pos, _) : node) : bool = Matrix.get g pos = 'E'
end

module Search = Search.Make (Graph)

let start (g : Graph.t) : Graph.node =
  let (Some start) = Matrix.find (( = ) 'S') g in
  (start, Coord.Right)

let part_one input = Search.find_shortest_path input [ start input ]

(*
 * Part 2
 *)
let ends (g : Graph.t) : Graph.node list =
  let (Some end_) = Matrix.find (( = ) 'E') g in
  [ (end_, Coord.Left); (end_, Coord.Down) ]

(* Flips all directions in the search history *)
let flip_dir (m : 'a Search.NodeMap.t) : 'a Search.NodeMap.t =
  let open Search.NodeMap in
  let collect (c, d) v m' = add (c, Coord.flip d) v m' in
  fold collect m empty

(* Counts the unique coordinates in a search history, ignoring direction. *)
let unique_coords (m : 'a Search.NodeMap.t) : int =
  let open Coord.Set in
  let collect (c, _) _ s = add c s in
  Search.NodeMap.fold collect m empty |> cardinal

let count_reachables (g : Graph.t) =
  let module NodeMap = Search.NodeMap in
  let costs_from p = Search.find_distance_from g p in
  (* Combine the costs from the start and the end (the end costs must be
   * flipped, because we traverse bacwkards): *)
  let from_start = costs_from [ start g ] in
  let from_end = costs_from (ends g) |> flip_dir in
  let combine_costs _ l r = Option.merge ( + ) l r in
  let combined_costs = NodeMap.merge combine_costs from_start from_end in
  (* Any ndoe with the minimum combined cost is on a shortest path: *)
  let min_cost = NodeMap.find (start g) from_end in
  let is_min_cost _ v = v = min_cost in
  NodeMap.filter is_min_cost combined_costs |> unique_coords

let part_two = count_reachables

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
