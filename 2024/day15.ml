open Utils
open Utils.Coord.Infix
open Utils.List.Infix

(*
 * Parse input
 *)
type input_t = { map : char Matrix.t; moves : Coord.dir list }

let dir_of_char (c : char) : Coord.dir =
  Coord.(match c with '^' -> Up | '>' -> Right | 'v' -> Down | '<' -> Left)

let parse_input (lines : string list) : input_t =
  let [ map; moves ] = Io.split_blocks lines in
  let map = Matrix.of_strings map in
  let moves = String.concat "" moves |> String.explode ||> dir_of_char in
  { map; moves }

let puzzle_input = Io.read_lines "2024/data/15.txt" |> parse_input

(*
 * Part 1
 *)
type warehouse_state = {
  walls : Coord.Set.t;
  boxes : Coord.Set.t;
  robot : Coord.t;
}

type init_fn = input_t -> warehouse_state
type move_fn = warehouse_state -> Coord.dir -> warehouse_state

let init_warehouse (input : input_t) : warehouse_state =
  let walls = Matrix.find_all (( = ) '#') input.map |> Coord.Set.of_list in
  let boxes = Matrix.find_all (( = ) 'O') input.map |> Coord.Set.of_list in
  let robot = Matrix.find (( = ) '@') input.map |> Option.get in
  { walls; boxes; robot }

let push_box (state : warehouse_state) (pos : Coord.t) (dir : Coord.dir) :
    warehouse_state option =
  (* Find the first free space after a line of boxes; move the closest box to
   * the open space. This is equivalent to pushing every box one space. *)
  let rec find_open_space (pos : Coord.t) : Coord.t option =
    if Coord.Set.contains state.walls pos then None
    else if Coord.Set.contains state.boxes pos then
      find_open_space (Coord.step dir pos)
    else Some pos
  in
  let move_box pos' =
    {
      walls = state.walls;
      boxes = Coord.Set.replace pos pos' state.boxes;
      robot = pos;
    }
  in
  find_open_space pos |> Option.map move_box

(* Returns the updated state after applying a single move. *)
let apply_move (state : warehouse_state) (dir : Coord.dir) : warehouse_state =
  let p = Coord.step dir state.robot in
  if Coord.Set.contains state.walls p then state
  else if Coord.Set.contains state.boxes p then
    match push_box state p dir with Some state' -> state' | None -> state
  else { walls = state.walls; boxes = state.boxes; robot = p }

(* Simulates a given input, returning the final state *)
let simulate (init : init_fn) (move : move_fn) (input : input_t) :
    warehouse_state =
  let simulate_inner state m = move state m in
  List.fold_left simulate_inner (init input) input.moves

(* Converts a coord to GPS *)
let gps_of_coord ((r, c) : Coord.t) : int = (100 * r) + c

(* Shared glue between both parts. *)
let compute_answer (init : init_fn) (move : move_fn) (input : input_t) : int =
  let state = simulate init move input in
  let acc c sum = sum + gps_of_coord c in
  Coord.Set.fold acc state.boxes 0

let part_one = compute_answer init_warehouse apply_move

(*
 * Part 2
 *)
let init_wide_warehouse (input : input_t) : warehouse_state =
  let new_coord (r, c) = (r, 2 * c) in
  let shifted_coord (r, c) = (r, (2 * c) + 1) in
  let { walls; boxes; robot } = init_warehouse input in
  let walls =
    Coord.Set.union
      (Coord.Set.map new_coord walls)
      (Coord.Set.map shifted_coord walls)
  in
  let boxes = Coord.Set.map new_coord boxes in
  { walls; boxes; robot = new_coord robot }

let get_box (state : warehouse_state) (pos : Coord.t) : Coord.t option =
  let find p = Coord.Set.find_opt p state.boxes in
  Option.or_ (find pos) (find (pos -- (0, 1)))

(* Tries to push a wide box, returning the new state after pushing. *)
let rec push_wide_box (state : warehouse_state) (pos : Coord.t)
    (dir : Coord.dir) : warehouse_state option =
  let push_half pos state =
    if Coord.Set.contains state.walls pos then None
    else
      match get_box state pos with
      | Some pos' -> push_wide_box state pos' dir
      | None -> Some state
  in
  let pos' = Coord.step dir pos in
  let move_box state =
    Some
      {
        walls = state.walls;
        boxes = Coord.Set.replace pos pos' state.boxes;
        robot = pos;
      }
  in
  (match dir with
  | Coord.Up | Coord.Down ->
      push_half pos' state
      |> Option.then_ (push_half (pos' ++ (0, 1)))
      |> Option.then_ move_box
  | Coord.Left -> push_half pos' state
  | Coord.Right -> push_half (pos' ++ (0, 1)) state)
  |> Option.then_ move_box

let apply_wide_move (state : warehouse_state) (dir : Coord.dir) :
    warehouse_state =
  let p = Coord.step dir state.robot in
  if Coord.Set.contains state.walls p then state
  else
    match get_box state p with
    | None -> { walls = state.walls; boxes = state.boxes; robot = p }
    | Some b -> (
        match push_wide_box state b dir with
        | Some { walls; boxes; _ } -> { walls; boxes; robot = p }
        | None -> state)

let part_two = compute_answer init_wide_warehouse apply_wide_move

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
