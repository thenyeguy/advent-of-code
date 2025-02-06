open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/21.txt"

(*
 * Part 1
 *)
let run_program (prog : Intcode.Computer.program_t) (script : string list) : int
    =
  let module A = Intcode.Ascii in
  let module C = Intcode.Computer in
  (* Start and flush prompt: *)
  let c = C.init prog in
  ignore (C.run c |> A.read_all);
  (* Input jumpscript: *)
  List.iter (A.write_line c) script;
  (* Run, and flush output: *)
  ignore (C.run c);
  ignore (A.read_line c);
  ignore (A.read_line c);
  ignore (A.read_line c);
  (* Fetch result: *)
  if Queue.length c.outputs = 1 then C.pop c
  else
    let err = A.read_all c in
    print_endline err;
    raise (Failure "springdroid fell")

let part_one input =
  run_program input
    [
      (* Jump if A is a hole: *)
      "NOT A J";
      (* Jump if early if B or C is a hole and D is ground: *)
      "NOT T T";
      "AND B T";
      "AND C T";
      "NOT T T";
      "AND D T";
      "OR T J";
      (* Walk: *)
      "WALK";
    ]

(*
 * Part 2
 *)
let part_two input =
  run_program input
    [
      (* Jump if A is a hole: *)
      "NOT A J";
      (* Jump if early if B or C is a hole and D AND H are ground: *)
      "NOT T T";
      "AND B T";
      "AND C T";
      "NOT T T";
      "AND D T";
      "AND H T";
      "OR T J";
      (* Run: *)
      "RUN";
    ]

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
