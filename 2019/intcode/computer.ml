exception Halt

type program_t = int list
type computer_t = { mem : int array; mutable pc : int }

let init (program : program_t) : computer_t =
  { mem = Array.of_list program; pc = 0 }

let step (c : computer_t) =
  match c.mem.(c.pc) with
  | 1 (* add *) ->
      let a = c.mem.(c.pc + 1) in
      let b = c.mem.(c.pc + 2) in
      let idx = c.mem.(c.pc + 3) in
      c.mem.(idx) <- c.mem.(a) + c.mem.(b);
      c.pc <- c.pc + 4
  | 2 (* mul *) ->
      let a = c.mem.(c.pc + 1) in
      let b = c.mem.(c.pc + 2) in
      let idx = c.mem.(c.pc + 3) in
      c.mem.(idx) <- c.mem.(a) * c.mem.(b);
      c.pc <- c.pc + 4
  | 99 (* halt *) -> raise Halt
  | op -> raise (Failure (Format.sprintf "unknown instruction: %d" op))

let rec run_to_completion (c : computer_t) =
  try
    step c;
    run_to_completion c
  with Halt -> ()

let run (program : program_t) : int =
  let computer = init program in
  let () = run_to_completion computer in
  computer.mem.(0)
