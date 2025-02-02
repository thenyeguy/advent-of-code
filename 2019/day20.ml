open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2019/data/20.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
module StringMap = Map.Make (String)

type graph = {
  aa : coord;
  zz : coord;
  portals : coord Coord.Map.t;
  m : char matrix;
}

let get_portal (m : char matrix) (pos : coord) : string option =
  let fetch (d : dir) : string option =
    let c = Matrix.get m (Coord.step pos d) in
    if 'A' <= c && c <= 'Z' then
      let c2 = Matrix.get m (Coord.step ~steps:2 pos d) in
      match d with
      | Coord.Up | Coord.Left -> Some (String.implode [ c2; c ])
      | Coord.Down | Coord.Right -> Some (String.implode [ c; c2 ])
    else None
  in
  if Matrix.get m pos = '.' then
    List.map fetch Coord.dirs |> List.fold_left Option.or_ None
  else None

let get_portals (m : char matrix) : coord list StringMap.t =
  let get pos _ = get_portal m pos in
  let acc map (pos, portal) = StringMap.add_to_list portal pos map in
  Matrix.findi_all_map get m |> List.fold_left acc StringMap.empty

let build_graph (m : char matrix) : graph =
  let portals_by_name = get_portals m in
  let get portal = StringMap.find portal portals_by_name |> List.hd in
  let aa = get "AA" in
  let zz = get "ZZ" in
  let acc _ portals map =
    match portals with
    | [ c1; c2 ] -> map |> Coord.Map.add c1 c2 |> Coord.Map.add c2 c1
    | _ -> map
  in
  let portals = StringMap.fold acc portals_by_name Coord.Map.empty in
  { aa; zz; portals; m }

let shortest_path (g : graph) : int =
  let module Graph = struct
    type node = coord
    type t = graph

    let neighbors { portals; m; _ } (pos : node) : node list =
      let is_open p = Matrix.get m p = '.' in
      let adjacencies = Coord.adjacencies pos |> List.filter is_open in
      match Coord.Map.find_opt pos portals with
      | Some c -> c :: adjacencies
      | None -> adjacencies

    let cost _ _ _ = 1
    let is_done { zz; _ } (pos : node) : bool = pos = zz
  end in
  let module Search = Search.Make (Graph) in
  Search.find_shortest_path g [ g.aa ]

let part_one = build_graph >> shortest_path

(*
 * Part 2
 *)
let is_outer_portal (g : graph) ((r, c) : coord) : bool =
  let rows, cols = Matrix.size g.m in
  r = 2 || r = rows - 3 || c = 2 || c = cols - 3

let shortest_path_recursive (g : graph) : int =
  let module Graph = struct
    type node = coord * int
    type t = graph

    let neighbors { portals; m; _ } ((pos, level) : node) : node list =
      let is_open p = Matrix.get m p = '.' in
      let adjacencies =
        Coord.adjacencies pos |> List.filter is_open ||> Pair.rpack level
      in
      match Coord.Map.find_opt pos portals with
      | Some c ->
          (* If we have teleported to the outside, we have gone deeper.
           * Otherwise, we have come out (if we weren't already at the top *)
          let level = if is_outer_portal g c then level + 1 else level - 1 in
          if level < 0 then adjacencies else (c, level) :: adjacencies
      | None -> adjacencies

    let cost _ _ _ = 1
    let is_done { zz; _ } (n : node) : bool = n = (zz, 0)
  end in
  let module Search = Search.Make (Graph) in
  Search.find_shortest_path g [ (g.aa, 0) ]

let part_two = build_graph >> shortest_path_recursive

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
