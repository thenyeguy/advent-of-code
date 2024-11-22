open Utils
open Utils.List.Infix

(*
 * Types
 *)
type map = char Matrix.t
type edge_map = (Coord.t * int) list Coord.Map.t

module Search = Search.Make (Coord)

let print_coord (r, c) = Printf.printf "(%d,%d)" r c

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2023/data/23.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let neighbors ?(climb : bool = false) (map : map) (c : Coord.t) : Coord.t list =
  let is_valid_target d =
    match Matrix.get_opt map (Coord.step d c) with
    | Some '#' -> false
    | None -> false
    | _ -> true
  in
  let is_valid_dir d =
    climb
    || Coord.(
         match Matrix.get map c with
         | '^' -> d = Up
         | '<' -> d = Left
         | '>' -> d = Right
         | 'v' -> d = Down
         | _ -> true)
  in
  Coord.[ Up; Left; Down; Right ]
  |> List.filter is_valid_target
  |> List.filter is_valid_dir
  ||> fun d -> Coord.step d c

let get_longest_path (map : map) : int =
  let start, goal = ((0, 1), (Matrix.rows map - 1, Matrix.cols map - 2)) in
  let cost _ _ = 1 in
  let is_done c = c = goal in
  Search.find_longest_path (neighbors map) cost is_done start

let part_one = get_longest_path

(*
 * Part 2
 *)
let find_intersection (map : map) (src : Coord.t) (start : Coord.t) :
    Coord.t * int =
  let rec find_intersection' from c steps =
    match neighbors ~climb:true map c |> List.filter (( <> ) from) with
    | [ next ] -> find_intersection' c next (steps + 1)
    | _ -> (c, steps)
  in
  find_intersection' src start 1

let get_edges (map : map) : (Coord.t * Coord.t * int) list =
  let rec bfs seen edges frontier =
    match frontier with
    | [] -> edges
    | node :: frontier' ->
        if Coord.Set.mem node seen then bfs seen edges frontier'
        else
          let seen' = Coord.Set.add node seen in
          let nodes =
            neighbors ~climb:true map node ||> find_intersection map node
          in
          let edges' = nodes ||> fun (d, c) -> (node, d, c) in
          let nodes' = nodes ||> Pair.left in
          bfs seen' (edges @ edges') (frontier' @ nodes')
  in
  bfs Coord.Set.empty [] [ (0, 1) ]

let make_edge_map (edges : (Coord.t * Coord.t * int) list) : edge_map =
  let add_edge edges (src, dest, cost) =
    let append e existing =
      match existing with
      | None -> Option.some [ e ]
      | Some es -> Option.some (e :: es)
    in
    Coord.Map.update src (append (dest, cost)) edges
  in
  List.fold_left add_edge Coord.Map.empty edges

let get_longest_path2 (map : map) : int =
  let edges = map |> get_edges |> make_edge_map in
  let start, goal = ((0, 1), (Matrix.rows map - 1, Matrix.cols map - 2)) in
  let neighbors (node : Coord.t) = Coord.Map.find node edges ||> Pair.left in
  let cost (src : Coord.t) (dest : Coord.t) : int =
    edges |> Coord.Map.find src |> List.assoc dest
  in
  let is_done c = c = goal in
  Search.find_longest_path neighbors cost is_done start

let part_two = get_longest_path2

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
