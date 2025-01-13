open Utils
module CoordSet = Set.Make (Coord)

module DirMap = Map.Make (struct
  type t = Coord.dir

  let compare = compare
end)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/12.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let neighbors (m : char Matrix.t) (c : Coord.t) : Coord.t list =
  let is_connected c' = Matrix.get_opt m c = Matrix.get_opt m c' in
  List.filter is_connected (Coord.adjacencies c)

let components (m : char Matrix.t) : CoordSet.t list =
  let rec dfs (seen : CoordSet.t) (frontier : Coord.t list) : CoordSet.t =
    match frontier with
    | [] -> seen
    | c :: cs ->
        if CoordSet.contains seen c then dfs seen cs
        else
          let ns = neighbors m c in
          let seen' = CoordSet.add c seen in
          dfs seen' (ns @ cs)
  in
  let rec components_inner (frontier : CoordSet.t) : CoordSet.t list =
    match CoordSet.choose_opt frontier with
    | None -> []
    | Some c ->
        let component = dfs CoordSet.empty [ c ] in
        let frontier' = CoordSet.diff frontier component in
        component :: components_inner frontier'
  in
  Matrix.coords m |> CoordSet.of_list |> components_inner

let component_cost (m : char Matrix.t) (component : CoordSet.t) : int =
  let acc c p = p + 4 - (neighbors m c |> List.length) in
  let perimeter = CoordSet.fold acc component 0 in
  let area = CoordSet.cardinal component in
  area * perimeter

let part_one input = components input ||> component_cost input |> List.sum

(*
 * Part 2
 *)
let component_sides (component : CoordSet.t) : int =
  (* Gets all edge normals around the component. *)
  let get_edges (cs : CoordSet.t) : Coord.t list DirMap.t =
    let edges' (cs : CoordSet.t) (c : Coord.t) : (Coord.dir * Coord.t) list =
      let is_open dir =
        if Coord.step c dir |> CoordSet.contains cs then None else Some (dir, c)
      in
      List.filter_map is_open Coord.dirs
    in
    let acc m (d, c) = DirMap.add_to_list d c m in
    CoordSet.to_list cs ||> edges' cs |> List.flatten
    |> List.fold_left acc DirMap.empty
  in
  (* Counts contiguous segments in a list. *)
  let get_segments (ls : int list) : int =
    let rec segments_inner ls =
      match ls with
      | l1 :: l2 :: ls when l1 + 1 = l2 -> segments_inner (l2 :: ls)
      | _ :: ls -> 1 + segments_inner ls
      | [] -> 0
    in
    List.sort compare ls |> segments_inner
  in
  (* Counts all sides facing a specific direction. *)
  let count_sides (dir : Coord.dir) (cs : Coord.t list) : int =
    (* Flip the major axis so that it is always normal to the edge *)
    let cs =
      Coord.(
        match dir with Up | Down -> cs | Left | Right -> List.map Pair.swap cs)
    in
    (* Group all edges by their major axis, then count how many contiguous
       segments there are. Each of these is one side. *)
    let same_axis (l1, _) (l2, _) = l1 = l2 in
    List.sort compare cs |> List.group same_axis ||> List.map Pair.right
    ||> get_segments |> List.sum
  in
  let acc dir cs sides = sides + count_sides dir cs in
  let edges = get_edges component in
  DirMap.fold acc edges 0

let discount_cost (component : CoordSet.t) : int =
  let area = CoordSet.cardinal component in
  let sides = component_sides component in
  area * sides

let part_two input = components input ||> discount_cost |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
