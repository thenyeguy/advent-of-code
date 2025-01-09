open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2019/data/10.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let can_see (asteroids : Coord.Set.t) (a : coord) (b : coord) : bool =
  let open Coord.Infix in
  if a = b then false
  else
    let dx, dy = b -- a in
    let gcd = Math.gcd (abs dx) (abs dy) in
    if gcd = 1 then true
    else
      let v = (dx / gcd, dy / gcd) in
      let rec step p =
        let p = p ++ v in
        if p = b then true
        else if Coord.Set.contains asteroids p then false
        else step p
      in
      step a

let find_asteroids (m : char matrix) : coord list =
  Matrix.find_all (( = ) '#') m

let count_visible (asteroids : coord list) : int Coord.Map.t =
  let asteroid_set = Coord.Set.of_list asteroids in
  let count_visible_inner m a =
    let count = List.count (can_see asteroid_set a) asteroids in
    Coord.Map.add a count m
  in
  List.fold_left count_visible_inner Coord.Map.empty asteroids

let max_asteroid (counts : int Coord.Map.t) : coord * int =
  let find_max coord count (max_coord, max_count) =
    if count > max_count then (coord, count) else (max_coord, max_count)
  in
  Coord.Map.fold find_max counts ((0, 0), 0)

let part_one = find_asteroids >> count_visible >> max_asteroid >> snd

(*
 * Part 2
 *)
let vaporize (asteroids : Coord.Set.t) (pos : coord) : Coord.Set.t =
  Coord.Set.filter (can_see asteroids pos) asteroids

(* Angle of vec from a to b, with 0 radians meaning up *)
let angle ((xa, ya) : coord) ((xb, yb) : coord) : float =
  let x = float_of_int (xb - xa) in
  let y = float_of_int (yb - ya) in
  let tan = Float.atan2 y x in
  Float.pi -. tan

let sort_by_angle (pos : coord) (asteroids : coord list) : coord list =
  let cmp a b = compare (angle pos a) (angle pos b) in
  List.sort cmp asteroids

let find_nth_asteroid ?(n : int = 200) (pos : coord) (asteroids : coord list) :
    coord =
  let asteroids = Coord.Set.of_list asteroids in
  let rec find asteroids count =
    let vaporized = vaporize asteroids pos in
    let count' = count + Coord.Set.cardinal vaporized in
    if count' < n then find (Coord.Set.diff asteroids vaporized) count
    else
      let sorted = Coord.Set.to_list vaporized |> sort_by_angle pos in
      List.nth sorted (n - count - 1)
  in
  find asteroids 0

let part_two input =
  let asteroids = find_asteroids input in
  let pos, _ = asteroids |> count_visible |> max_asteroid in
  let y, x = find_nth_asteroid pos asteroids in
  (100 * x) + y

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
