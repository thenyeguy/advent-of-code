open Utils
open Utils.List.Infix

(*
 * Types
 *)
type coord = int * int (* row, col *)
type dir = Up | Down | Left | Right
type beam = { coord : coord; dir : dir }

module CoordSet = Set.Make (struct
  type t = coord

  let compare = compare
end)

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "data/16.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let in_bounds (tiles : char Matrix.t) (b : beam) : bool =
  let r, c = b.coord in
  Matrix.in_bounds tiles r c

let step (tiles : char Matrix.t) (b : beam) : beam list =
  let r, c = b.coord in
  let move dir =
    match dir with
    | Up -> { coord = (r - 1, c); dir }
    | Down -> { coord = (r + 1, c); dir }
    | Left -> { coord = (r, c - 1); dir }
    | Right -> { coord = (r, c + 1); dir }
  in
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
  match Matrix.get tiles r c with
  | '.' -> [ move b.dir ]
  | '/' -> [ bounce '/' b.dir ]
  | '\\' -> [ bounce '\\' b.dir ]
  | '-' -> split '-' b.dir
  | '|' -> split '|' b.dir

let travel (tiles : char Matrix.t) (start : beam) : CoordSet.t =
  let seen = Hashtbl.create 100 in
  let rec travel' coords b =
    let coords' = CoordSet.add b.coord coords in
    if Hashtbl.mem seen b then coords'
    else
      let _ = Hashtbl.add seen b () in
      let beams = step tiles b |> List.filter (in_bounds tiles) in
      List.fold_left travel' coords' beams
  in
  travel' CoordSet.empty start

let part_one (tiles : char Matrix.t) : int =
  travel tiles { coord = (0, 0); dir = Right } |> CoordSet.cardinal

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
  starting_beams tiles ||> travel tiles ||> CoordSet.cardinal |> List.max

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
