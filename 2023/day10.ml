open Utils

(*
 * Types
 *)
type map = char matrix

(*
 * Parse input
 *)
let parse_map = Matrix.of_strings
let puzzle_input = Io.read_lines "2023/data/10.txt" |> parse_map

(*
 * Part 1
 *)
let rec is_adjacent (map : map) (c1 : coord) (c2 : Coord.t) : bool =
  try adjacencies map c2 |> List.mem c1 with Invalid_argument _ -> false

and adjacencies (map : map) ((r, c) : coord) : Coord.t list =
  match Matrix.get map (r, c) with
  | 'S' ->
      (* work backwards from starting spot to find adjacencies *)
      [ (r - 1, c); (r, c - 1); (r, c + 1); (r + 1, c) ]
      |> List.filter (is_adjacent map (r, c))
  | '|' -> [ (r - 1, c); (r + 1, c) ]
  | '-' -> [ (r, c - 1); (r, c + 1) ]
  | 'L' -> [ (r - 1, c); (r, c + 1) ]
  | 'J' -> [ (r - 1, c); (r, c - 1) ]
  | '7' -> [ (r, c - 1); (r + 1, c) ]
  | 'F' -> [ (r, c + 1); (r + 1, c) ]
  | _ -> []

(* Counts how many steps from the start each reachable pipe is. *)
let count_steps (map : map) : int Coord.Map.t =
  let rec bfs (frontier : coord list) (seen : int Coord.Map.t) (steps : int) =
    if List.is_empty frontier then seen
    else
      let is_unseen c = not (Coord.Map.mem c seen) in
      let frontier' =
        frontier ||> adjacencies map |> List.flatten |> List.filter is_unseen
      in
      let seen' = frontier ||> (fun c -> (c, steps)) |> Coord.Map.of_list in
      let map_merge _ l r = Some (min l r) in
      bfs frontier' (Coord.Map.union map_merge seen seen') (steps + 1)
  in
  let (Some start) = Matrix.find (( = ) 'S') map in
  bfs [ start ] (Coord.Map.singleton start 0) 0

let part_one (map : map) : int =
  map |> count_steps |> Coord.Map.to_list ||> snd |> List.fold_left max 0

(*
 * Part 2
 *)

(* Filters any pipes that can't be reached from the map. *)
let remove_unreachable_pipes (map : map) : map =
  let seen = count_steps map in
  let new_char row col char =
    let c = (row, col) in
    if Coord.Map.mem c seen then char else '.'
  in
  Matrix.mapi new_char map

(* Doubles the size of the map, adding dummy pipes to maintain connections. *)
let expand_map (map : map) : map =
  let rows, cols = (Matrix.rows map, Matrix.cols map) in
  let map' = Matrix.make ((2 * rows) + 1) ((2 * cols) + 1) '.' in
  let copy_pipe r c ch =
    let dummy_pipe (r2, c2) = map'.(r2).(c2) <- '+' in
    let r', c' = ((2 * r) + 1, (2 * c) + 1) in
    map'.(r').(c') <- ch;
    adjacencies map' (r', c') |> List.iter dummy_pipe
  in
  Matrix.iteri copy_pipe map;
  map'

(* Fills all tiles reachable from outside the pipe with xs. *)
let flood_fill (map : map) : map =
  let map = Matrix.copy map in
  let flood c =
    try
      if Matrix.get map c = '.' then (
        Matrix.set map c 'x';
        Coord.adjacencies c)
      else []
    with Invalid_argument _ -> []
  in
  let rec bfs (frontier : coord list) =
    if not (List.is_empty frontier) then bfs (frontier ||> flood |> List.flatten)
  in
  bfs [ (0, 0) ];
  map

(* Counts all unreached tiles from the pre-expanded map. *)
let count_enclosed_tiles (map : map) : int =
  let count row col ch =
    if ch = '.' && row mod 2 = 1 && col mod 2 = 1 then 1 else 0
  in
  map |> Matrix.mapi count |> Matrix.fold ( + ) 0

let part_two (input : map) : int =
  input |> remove_unreachable_pipes |> expand_map |> flood_fill
  |> count_enclosed_tiles

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
