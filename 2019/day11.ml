open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/11.txt"

(*
 * Part 1
 *)
type robot_t = { pos : coord; dir : dir }

let update (robot : robot_t) (colors : int Coord.Map.t) (color : int)
    (turn : int) : robot_t * int Coord.Map.t =
  let open Coord in
  (* Paint, then step: *)
  let colors = Map.add robot.pos color colors in
  let dir =
    match turn with 0 -> turn_left robot.dir | 1 -> turn_right robot.dir
  in
  let pos = step robot.pos dir in
  ({ pos; dir }, colors)

let step (c : Intcode.Computer.t) (robot : robot_t) (colors : int Coord.Map.t) :
    robot_t * int Coord.Map.t =
  let open Intcode.Computer in
  let color = Coord.Map.find_or robot.pos 0 colors in
  push c color;
  ignore (run c);
  let color = pop c in
  let turn = pop c in
  update robot colors color turn

let paint_hull ?(initial : int = 0) (prog : Intcode.Computer.program_t) :
    int Coord.Map.t =
  let c = Intcode.Computer.init prog in
  let rec paint (robot, colors) =
    if Intcode.Computer.is_done c then colors else step c robot colors |> paint
  in
  paint ({ pos = (0, 0); dir = Coord.Up }, Coord.Map.singleton (0, 0) initial)

let part_one = paint_hull >> Coord.Map.cardinal

(*
 * Part 2
 *)
let visualize (m : int Coord.Map.t) : string =
  let black_coords (coord, color) = if color = 1 then Some coord else None in
  let cs = Coord.Map.to_list m |> List.filter_map black_coords in
  let min = (cs ||> fst |> List.min, cs ||> snd |> List.min) in
  let max = (cs ||> fst |> List.max, cs ||> snd |> List.max) in
  let rows, cols = Coord.Infix.(max -- min ++ (1, 1)) in
  let m = Matrix.make rows cols ' ' in
  let paint_black c =
    let row, col = Coord.Infix.(c -- min) in
    m.(row).(col) <- '#'
  in
  List.iter paint_black cs;
  Matrix.to_string m

let part_two = paint_hull ~initial:1 >> visualize >> String.cat "\n"

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input (part_one >> string_of_int) part_two
