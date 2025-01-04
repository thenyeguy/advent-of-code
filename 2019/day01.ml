open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2019/data/01.txt" ||> int_of_string

(*
 * Part 1
 *)
let fuel_cost (mass : int) : int = (mass / 3) - 2
let part_one = List.map fuel_cost >> List.sum

(*
 * Part 2
 *)
let rec recursive_fuel_cost (mass : int) : int =
  let cost = fuel_cost mass in
  if cost <= 0 then 0 else cost + recursive_fuel_cost cost

let part_two = List.map recursive_fuel_cost >> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
