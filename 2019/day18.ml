open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2019/data/18.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
module CharMap = Map.Make (Char)

(* A specialized set that stores characters in a packed integer. *)
module CharSet = struct
  open Int

  type t = int

  let empty : t = 0
  let zero : int = int_of_char 'a'
  let idx (c : char) : int = int_of_char c - zero
  let is_empty (s : t) : bool = s = 0
  let add (c : char) (s : t) : t = shift_left 1 (idx c) |> logor s
  let diff (s : t) (other : t) : t = logand s (lognot other)

  let contains (s : t) (c : char) : bool =
    shift_right s (idx c) |> logand 1 |> ( = ) 1

  let rec cardinal (s : t) : int =
    if s = 0 then 0 else logand s 1 + cardinal (shift_right s 1)

  let fold (f : char -> 'acc -> 'acc) (s : t) (acc : 'acc) : 'acc =
    let rec fold_inner (s : t) (n : int) (acc : 'acc) : 'acc =
      if s = 0 then acc
      else
        let acc =
          if logand s 1 = 1 then f (char_of_int (n + zero)) acc else acc
        in
        fold_inner (shift_right s 1) (n + 1) acc
    in
    fold_inner s 0 acc

  let to_list (s : t) : char list = fold List.cons s []
end

let is_lower (c : char) : bool = 'a' <= c && c <= 'z'
let is_upper (c : char) : bool = 'A' <= c && c <= 'Z'

let neighbors (m : char matrix) (pos : coord) : coord list =
  let is_valid p =
    match Matrix.get_opt m p with Some c -> c <> '#' | _ -> false
  in
  Coord.adjacencies pos |> List.filter is_valid

let find_dependencies (starts : char list) (m : char matrix) :
    CharSet.t CharMap.t =
  let add_door (pos : coord) (doors : CharSet.t) : CharSet.t =
    let c = Matrix.get m pos in
    if is_upper c then CharSet.add (Char.lowercase_ascii c) doors else doors
  in
  let add_dep (pos : coord) (doors : CharSet.t) (deps : CharSet.t CharMap.t) :
      CharSet.t CharMap.t =
    let c = Matrix.get m pos in
    if is_lower c then CharMap.add c doors deps else deps
  in
  let rec bfs (seen : Coord.Set.t) (deps : CharSet.t CharMap.t)
      (frontier : (coord * CharSet.t) list) : CharSet.t CharMap.t =
    match frontier with
    | [] -> deps
    | (pos, doors) :: frontier ->
        if Coord.Set.contains seen pos then bfs seen deps frontier
        else
          let seen = Coord.Set.add pos seen in
          let deps = add_dep pos doors deps in
          let doors = add_door pos doors in
          let frontier' = neighbors m pos ||> Pair.rpack doors in
          bfs seen deps (frontier' @ frontier)
  in
  let get_start c = (Matrix.find (( = ) c) m |> Option.get, CharSet.empty) in
  bfs Coord.Set.empty CharMap.empty (List.map get_start starts)

let find_distances ?(max : char = 'z') (starts : char list) (m : char matrix) :
    int CharMap.t CharMap.t =
  let module Graph = struct
    type node = coord
    type t = unit

    let neighbors _ (pos : coord) : coord list = neighbors m pos
    let cost _ _ _ = 1
    let is_done _ _ = false
  end in
  let module Search = Search.Make (Graph) in
  let extract_distance (ds : int Search.NodeMap.t) : int CharMap.t =
    let get_keys (k, v) =
      let c = Matrix.get m k in
      if is_lower c then Some (c, v) else None
    in
    Search.NodeMap.to_list ds |> List.filter_map get_keys |> CharMap.of_list
  in
  let find_distances_inner (c : char) : char * int CharMap.t =
    Search.find_distance_from () [ Matrix.find (( = ) c) m |> Option.get ]
    |> extract_distance |> Pair.pack c
  in
  starts @ Char.irange 'a' max ||> find_distances_inner |> CharMap.of_list

let shortest_path_single ?(max : char = 'z') (m : char matrix) : int =
  let module Graph = struct
    (* A key, and the keys we have *)
    type node = char * CharSet.t

    (* Key dependencies, and distances between keys. *)
    type t = CharSet.t CharMap.t * int CharMap.t CharMap.t

    let neighbors ((deps, _) : t) ((_, keys) : node) : node list =
      let is_valid k ds =
        CharSet.contains keys k |> not
        && CharSet.diff ds keys |> CharSet.is_empty
      in
      let make_node k = (k, CharSet.add k keys) in
      CharMap.filter is_valid deps |> CharMap.keys ||> make_node

    let cost ((_, dist) : t) ((src, _) : node) ((dst, _) : node) : int =
      CharMap.find src dist |> CharMap.find dst

    let is_done ((deps, _) : t) ((_, keys) : node) : bool =
      CharSet.cardinal keys = CharMap.cardinal deps
  end in
  let module Search = Search.Make (Graph) in
  let deps = find_dependencies [ '@' ] m in
  let dists = find_distances ~max [ '@' ] m in
  Search.find_shortest_path (deps, dists) [ ('@', CharSet.empty) ]

let part_one = shortest_path_single

(*
 * Part 2
 *)
let replace_center (m : char matrix) : char matrix =
  let open Coord.Infix in
  let open Matrix in
  let rows, cols = size m in
  let center = (rows / 2, cols / 2) in
  let m = copy m in
  set m center '#';
  set m (center -- (0, 1)) '#';
  set m (center ++ (0, 1)) '#';
  set m (center -- (1, 0)) '#';
  set m (center ++ (1, 0)) '#';
  set m (center ++ (-1, -1)) '1';
  set m (center ++ (-1, 1)) '2';
  set m (center ++ (1, -1)) '3';
  set m (center ++ (1, 1)) '4';
  m

module Quadruple = struct
  type 'a t = 'a * 'a * 'a * 'a

  let of_list (l : 'a list) : 'a t =
    let [ a; b; c; d ] = l in
    (a, b, c, d)

  let map2 (f : 'a -> 'b -> 'c) ((al, bl, cl, dl) : 'a t)
      ((ar, br, cr, dr) : 'b t) =
    (f al ar, f bl br, f cl cr, f dl dr)

  let concat_map (f : 'a -> 'a list) ((a, b, c, d) : 'a t) : 'a t list =
    let as_ = List.map (fun a -> (a, b, c, d)) (f a) in
    let bs = List.map (fun b -> (a, b, c, d)) (f b) in
    let cs = List.map (fun c -> (a, b, c, d)) (f c) in
    let ds = List.map (fun d -> (a, b, c, d)) (f d) in
    as_ @ bs @ cs @ ds

  let fold (f : 'acc -> 'a -> 'acc) (acc : 'acc) ((a, b, c, d) : 'a t) : 'acc =
    let f' = Fn.flip f in
    acc |> f' a |> f' b |> f' c |> f' d
end

let shortest_path_multi ?(max : char = 'z') (m : char matrix) : int =
  let module Graph = struct
    (* The key each robot is on, and the keys we have found. *)
    type node = char Quadruple.t * CharSet.t

    (* Key dependencies, and distances between keys. *)
    type t = CharSet.t CharMap.t * int CharMap.t CharMap.t

    let neighbors ((deps, reachables) : t) ((robots, keys) : node) : node list =
      let is_valid k =
        CharSet.contains keys k |> not
        && CharSet.diff (CharMap.find k deps) keys |> CharSet.is_empty
      in
      let neighbors c =
        CharMap.find c reachables |> CharMap.keys |> List.filter is_valid
      in
      let pack (a, b, c, d) =
        let add s c = if is_lower c then CharSet.add c s else s in
        ((a, b, c, d), List.fold_left add keys [ a; b; c; d ])
      in
      Quadruple.concat_map neighbors robots ||> pack

    let cost ((_, dist) : t) ((as_, _) : node) ((bs, _) : node) : int =
      let cost_inner l r =
        if l = r then 0 else CharMap.find l dist |> CharMap.find r
      in
      Quadruple.map2 cost_inner as_ bs |> Quadruple.fold ( + ) 0

    let is_done ((deps, _) : t) ((_, keys) : node) : bool =
      CharSet.cardinal keys = CharMap.cardinal deps
  end in
  let module Search = Search.Make (Graph) in
  let deps = find_dependencies [ '1'; '2'; '3'; '4' ] m in
  let dists = find_distances ~max [ '1'; '2'; '3'; '4' ] m in
  Search.find_shortest_path (deps, dists)
    [ (('1', '2', '3', '4'), CharSet.empty) ]

let dep_list deps =
  let to_list (c, s) = (c, CharSet.to_list s) in
  CharMap.to_list deps ||> to_list

let cost_list costs =
  let to_list (c, cs) = (c, CharMap.to_list cs) in
  CharMap.to_list costs ||> to_list

let part_two = replace_center >> shortest_path_multi

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
