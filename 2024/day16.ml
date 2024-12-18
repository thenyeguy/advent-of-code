open Utils

module Search = Search.Make (struct
  type t = Coord.t * Coord.dir

  let compare = compare
end)

module IntMap = Map.Make (Int)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/16.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let start (m : char Matrix.t) : Search.node =
  let (Some start) = Matrix.find (( = ) 'S') m in
  (start, Coord.Right)

let neighbors (m : char Matrix.t) ((pos, dir) : Search.node) : Search.node list
    =
  let turns = [ (pos, Coord.turn_left dir); (pos, Coord.turn_right dir) ] in
  let step = Coord.step dir pos in
  match Matrix.get_opt m step with
  | Some '.' | Some 'S' | Some 'E' -> (step, dir) :: turns
  | _ -> turns

let cost ((_, src_dir) : Search.node) ((_, dest_dir) : Search.node) : int =
  if src_dir = dest_dir then 1 else 1000

let is_done (m : char Matrix.t) ((pos, _) : Search.node) : bool =
  Matrix.get m pos = 'E'

let part_one input =
  Search.find_shortest_path (neighbors input) cost (is_done input)
    [ start input ]

(*
 * Part 2
 *)
let ends (m : char Matrix.t) : Search.node list =
  let (Some end_) = Matrix.find (( = ) 'E') m in
  [ (end_, Coord.Left); (end_, Coord.Down) ]

(* Flips all directions in the search history *)
let flip_dir (m : 'a Search.HistoryMap.t) : 'a Search.HistoryMap.t =
  let open Search.HistoryMap in
  let collect (c, d) v m' = add (c, Coord.flip d) v m' in
  fold collect m empty

(* Counts the unique coordinates in a search history, ignoring direction. *)
let unique_coords (m : 'a Search.HistoryMap.t) : int =
  let open Coord.Set in
  let collect (c, _) _ s = add c s in
  Search.HistoryMap.fold collect m empty |> cardinal

let count_reachables (m : char Matrix.t) =
  let costs_from p = Search.find_distance_from (neighbors m) cost p in
  (* Combine the costs from the start and the end (the end costs must be
   * flipped, because we traverse bacwkards): *)
  let from_start = costs_from [ start m ] in
  let from_end = costs_from (ends m) |> flip_dir in
  let combine_costs _ l r = Some (Option.get l + Option.get r) in
  let combined_costs =
    Search.HistoryMap.merge combine_costs from_start from_end
  in
  (* Any ndoe with the minimum combined cost is on a shortest path: *)
  let min_cost = Search.HistoryMap.find (start m) from_end in
  let is_min_cost _ v = v = min_cost in
  Search.HistoryMap.filter is_min_cost combined_costs |> unique_coords

let part_two = count_reachables

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
