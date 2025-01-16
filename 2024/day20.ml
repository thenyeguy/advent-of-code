open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/20.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
module Graph = struct
  type t = char matrix
  type node = coord

  let starts (g : t) : node list = [ Matrix.find (( = ) 'S') g |> Option.get ]

  let is_open_space (g : t) (p : node) : bool =
    match Matrix.get_opt g p with Some '.' | Some 'E' -> true | _ -> false

  let neighbors (g : t) (pos : node) : node list =
    List.filter (is_open_space g) (Coord.adjacencies pos)

  let cost _ _ _ : int = 1
  let is_done _ _ : bool = false (* not needed fo find all costs *)
end

module IntMap = Map.Make (Int)
module Search = Search.Make (Graph)

let reachables ~(steps : int) (g : Graph.t) (pos : Graph.node) : Graph.node list
    =
  let reachables_inner x =
    let all_four y =
      [ (x, y); (x, -y); (-x, y); (-x, -y) ]
      |> List.sort_uniq compare ||> Coord.add pos
      |> List.filter (Graph.is_open_space g)
    in
    List.irange (steps - x) |> List.concat_map all_four
  in
  List.irange steps |> List.concat_map reachables_inner

(* Finds all cheats, keyed by home much time they save *)
let find_all_cheats ~(steps : int) (g : Graph.t) : int IntMap.t =
  let module NodeMap = Search.NodeMap in
  let node_costs = Search.find_distance_from g (Graph.starts g) in
  let acc_cheats src cost costs =
    let cheat_cost dst =
      let dist = Coord.manhattan_distance src dst in
      let saving = NodeMap.find dst node_costs - cost - dist in
      if saving > 0 then Some saving else None
    in
    reachables ~steps g src |> List.filter_map cheat_cost
    |> List.fold_left IntMap.increment costs
  in
  NodeMap.fold acc_cheats node_costs IntMap.empty

let above_threshold ?(thresh : int = 100) (cheats : int IntMap.t) : int =
  let acc saving count total =
    if saving >= thresh then total + count else total
  in
  IntMap.fold acc cheats 0

let part_one = find_all_cheats ~steps:2 >> above_threshold

(*
 * Part 2
 *)
let part_two = find_all_cheats ~steps:20 >> above_threshold

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
