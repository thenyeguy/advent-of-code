open Utils

(*
 * Types
 *)
type beam = { coord : Coord.t; dir : Coord.dir }

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2023/data/16.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let in_bounds (tiles : char Matrix.t) (b : beam) : bool =
  Matrix.in_bounds tiles b.coord

let step (tiles : char Matrix.t) (b : beam) : beam list =
  let open Coord in
  let move dir = { coord = step b.coord dir; dir } in
  let bounce c dir =
    match (c, dir) with
    | '/', Up -> move Right
    | '/', Down -> move Left
    | '/', Left -> move Down
    | '/', Right -> move Up
    | '\\', Up -> move Left
    | '\\', Down -> move Right
    | '\\', Left -> move Up
    | '\\', Right -> move Down
  in
  let split c dir =
    match (c, dir) with
    | '-', Left -> [ move dir ]
    | '-', Right -> [ move dir ]
    | '-', _ -> [ move Left; move Right ]
    | '|', Up -> [ move dir ]
    | '|', Down -> [ move dir ]
    | '|', _ -> [ move Up; move Down ]
  in
  match Matrix.get tiles b.coord with
  | '.' -> [ move b.dir ]
  | '/' -> [ bounce '/' b.dir ]
  | '\\' -> [ bounce '\\' b.dir ]
  | '-' -> split '-' b.dir
  | '|' -> split '|' b.dir

let travel (tiles : char Matrix.t) (start : beam) : Coord.Set.t =
  let seen = Hashtbl.create 100 in
  let rec travel' coords b =
    let coords' = Coord.Set.add b.coord coords in
    if Hashtbl.mem seen b then coords'
    else
      let _ = Hashtbl.add seen b () in
      let beams = step tiles b |> List.filter (in_bounds tiles) in
      List.fold_left travel' coords' beams
  in
  travel' Coord.Set.empty start

let part_one (tiles : char Matrix.t) : int =
  travel tiles { coord = (0, 0); dir = Right } |> Coord.Set.cardinal

(*
 * Part 2
 *)
let starting_beams (tiles : char Matrix.t) : beam list =
  let rows, cols = Matrix.size tiles in
  let row_is, col_is = (List.range rows, List.range cols) in
  let make_row r cs dir = cs ||> fun c -> { coord = (r, c); dir } in
  let make_col c rs dir = rs ||> fun r -> { coord = (r, c); dir } in
  make_row 0 col_is Down
  @ make_row (rows - 1) col_is Up
  @ make_col 0 row_is Right
  @ make_col (cols - 1) row_is Left

let part_two (tiles : char Matrix.t) : int =
  starting_beams tiles ||> travel tiles ||> Coord.Set.cardinal |> List.max

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
