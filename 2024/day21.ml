open Utils
open Utils.Coord.Infix

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/21.txt"

(*
 * Part 1
 *)
let key_coord (key : char) : coord =
  match key with
  (* 7 8 9
   * 4 5 6
   * 1 2 3
   *   0 A *)
  | 'A' -> (0, 0)
  | '0' -> (0, 1)
  | '1' -> (1, 2)
  | '2' -> (1, 1)
  | '3' -> (1, 0)
  | '4' -> (2, 2)
  | '5' -> (2, 1)
  | '6' -> (2, 0)
  | '7' -> (3, 2)
  | '8' -> (3, 1)
  | '9' -> (3, 0)
  (*   ^ A
   * < v > *)
  | '^' -> (0, 1)
  | '<' -> (-1, 2)
  | 'v' -> (-1, 1)
  | '>' -> (-1, 0)

let valid_path (src : char) (path : char list) =
  match (src, path) with
  | 'A', '<' :: '<' :: _ -> false
  | '0', '<' :: _ -> false
  | '^', '<' :: _ -> false
  | '<', '^' :: _ -> false
  | '1', 'v' :: _ -> false
  | '4', 'v' :: 'v' :: _ -> false
  | '7', 'v' :: 'v' :: 'v' :: _ -> false
  | _ -> true

let button_presses self ((robot, keys) : int * char list) : int =
  let press (dst : char) (src : char) : int =
    let rows, cols = key_coord dst -- key_coord src in
    let r, rs = if cols < 0 then ('>', -cols) else ('<', cols) in
    let c, cs = if rows < 0 then ('v', -rows) else ('^', rows) in
    [
      List.repeated r rs @ List.repeated c cs @ [ 'A' ];
      List.repeated c cs @ List.repeated r rs @ [ 'A' ];
    ]
    |> List.filter (valid_path src)
    ||> (if robot = 1 then List.length else fun p -> self (robot - 1, p))
    |> List.min
  in
  List.pair_map press ('A' :: keys) |> List.sum

let button_presses_memo = Memo.memo_rec button_presses

let complexity (robots : int) (keys : string) : int =
  let presses = button_presses_memo (robots, String.explode keys) in
  let code = String.sub keys 0 3 |> int_of_string in
  presses * code

let complexities ~(robots : int) (codes : string list) : int =
  let acc total code = total + complexity robots code in
  List.fold_left acc 0 codes

let part_one = complexities ~robots:3

(*
 * Part 2
 *)
let part_two = complexities ~robots:26

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
