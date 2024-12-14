open Utils
open Utils.Coord.Infix
open Utils.List.Infix

(*
 * Parse input
 *)
type robot = { p : Coord.t; v : Coord.vec }

let parse_robot (line : string) : robot =
  Scanf.sscanf line "p=%d,%d v=%d,%d" (fun x y dx dy ->
      { p = (x, y); v = (dx, dy) })

let puzzle_input = Io.read_lines "2024/data/14.txt" ||> parse_robot

(*
 * Part 1
 *)
let width = 101
let height = 103

let step ?(width : int = width) ?(height : int = height) (r : robot) : robot =
  let teleport max n =
    let n' = n mod max in
    if n' < 0 then n' + max else n'
  in
  let x, y = r.p ++ r.v in
  { p = (teleport width x, teleport height y); v = r.v }

let simulate ?(width : int = width) ?(height : int = height) (steps : int)
    (rs : robot list) : robot list =
  Fn.repeat (List.map (step ~width ~height)) steps rs

let compute_safety_score ?(width : int = width) ?(height : int = height)
    (rs : robot list) : int =
  let midx, midy = (width / 2, height / 2) in
  let quadrant { p = x, y; _ } =
    if x < midx && y < midy then Some 0
    else if x < midx && y > midy then Some 1
    else if x > midx && y < midy then Some 2
    else if x > midx && y > midy then Some 3
    else None
  in
  let qs = [| 0; 0; 0; 0 |] in
  let acc r =
    match quadrant r with Some q -> qs.(q) <- qs.(q) + 1 | _ -> ()
  in
  List.iter acc rs;
  Array.fold_left ( * ) 1 qs

let part_one ?(width : int = width) ?(height : int = height) input =
  simulate ~width ~height 100 input |> compute_safety_score ~width ~height

(*
 * Part 2
 *)
let visualize (rs : robot list) : unit =
  let m = Matrix.make width height ' ' in
  let draw r = Matrix.set m r.p 'o' in
  List.iter draw rs;
  let print_row r =
    Array.iter print_char r;
    print_newline ()
  in
  Array.iter print_row (Matrix.transpose m)

let rec safety_scores (steps : int) (rs : robot list) : int list =
  if steps = 0 then []
  else compute_safety_score rs :: safety_scores (steps - 1) (List.map step rs)

let part_two input =
  (* The safety score is minimized when a lot of robots are centered in the
   * image. The theory behind this is that this will occur when the trunk of
   * the tree appears, and the robots are roughly uniformally split between all
   * four quadrants to form the branches.
   *
   * There are width*height possible frames, so the output must be periodic on
   * at most that many frames, so we can just inspect all frames for the lowest
   * safety score.
   *
   * I visually inspected the output to see that it actually produced a tree :)
   *)
  safety_scores (width * height) input |> List.mini |> Pair.left

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
