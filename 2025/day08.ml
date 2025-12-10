open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/08.txt"

(*
 * Part 1
 *)
module Point = struct
  type t = int * int * int

  let compare = compare
end

module PSet = Set.Make (Point)
module PMap = Map.Make (Point)

type point = Point.t

let origin = (0, 0, 0)

type edge_map = int PMap.t PMap.t

let to_list (edges : edge_map) =
  let to_list' (src, es) = (src, PMap.to_list es) in
  PMap.to_list edges ||> to_list'

let parse (s : string) : point =
  Scanf.sscanf s "%d,%d,%d" (fun x y z -> (x, y, z))

let dist_sq ((ax, ay, az) : point) ((bx, by, bz) : point) : int =
  let sq x = x * x in
  sq (ax - bx) + sq (ay - by) + sq (az - bz)

let sorted_edges (ps : point list) : (point * point) list =
  let add_dist (p1, p2) = (p1, p2, dist_sq p1 p2) in
  let compare (_, _, d1) (_, _, d2) = compare d1 d2 in
  let remove_dist (p1, p2, _) = (p1, p2) in
  List.combinations ps ||> add_dist |> List.sort compare ||> remove_dist

let singleton_parents (ps : point list) : point PMap.t =
  List.map (fun p -> (p, p)) ps |> PMap.of_list

let join_edge (parents : point PMap.t) ((a, b) : point * point) : point PMap.t =
  let a' = PMap.find a parents in
  let b' = PMap.find b parents in
  let update parent = if parent = b' then a' else parent in
  PMap.map update parents

let count_parents (parents : point PMap.t) : int list =
  let acc _ parent counts = PMap.increment counts parent in
  PMap.fold acc parents PMap.empty |> PMap.to_list ||> snd

let connect_edges ?(n : int = 1) (ps : point list) : int list =
  let rec connect n parents edges =
    if n = 0 then parents
    else
      let (edge :: edges') = edges in
      connect (n - 1) (join_edge parents edge) edges'
  in
  connect n (singleton_parents ps) (sorted_edges ps) |> count_parents

let component_score (components : int list) : int =
  (List.sort compare >> List.rev >> List.take 3 >> List.product) components

let part_one = List.map parse >> connect_edges ~n:1000 >> component_score

(*
 * Part 2
 *)
let connect_all (ps : point list) : point * point =
  let rec connect parents edges =
    let (edge :: edges') = edges in
    let parents' = join_edge parents edge in
    if List.length (count_parents parents') = 1 then edge
    else connect parents' edges'
  in
  connect (singleton_parents ps) (sorted_edges ps)

let cable_length (((x1, _, _), (x2, _, _)) : point * point) : int = x1 * x2
let part_two = List.map parse >> connect_all >> cable_length

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
