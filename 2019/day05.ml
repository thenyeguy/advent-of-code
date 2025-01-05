open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/05.txt"

(*
 * Part 1
 *)
let part_one program = Intcode.Computer.run_program program [ 1 ] |> List.hd

(*
 * Part 2
 *)
let part_two program = Intcode.Computer.run_program program [ 5 ] |> List.hd

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
