open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/02.txt"

(*
 * Part 1
 *)
let set_input (noun : int) (verb : int) (p : int list) : int list =
  let (hd :: _ :: _ :: tl) = p in
  hd :: noun :: verb :: tl

let run (program : Intcode.Computer.program_t) : int =
  Intcode.Computer.(init program [] |> run |> read_memory 0)

let part_one = set_input 12 2 >> run

(*
 * Part 2
 *)
let part_two program =
  let run_with (noun, verb) = set_input noun verb program |> run in
  let range = Seq.range 100 in
  let noun, verb =
    Seq.product range range
    |> Seq.find (run_with >> ( = ) 19690720)
    |> Option.get
  in
  (100 * noun) + verb

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
