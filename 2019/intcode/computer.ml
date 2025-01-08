(* Computer state *)
type state_t = Idle | WaitingForInput | Done

(* A mutable IntCode computer *)
type t = {
  mutable mem : int array;
  mutable pc : int;
  mutable relative_base : int;
  mutable state : state_t;
  mutable inputs : int list;
  mutable outputs : int list;
}

(* A single IntCode Program *)
type program_t = int list

(* An IntCode parameter type. *)
type param_t = Position of int | Immediate of int | Relative of int

(* Raised to stop execution. *)
exception Interrupt

(* Initializes an IntCode computer. *)
let init (program : program_t) (inputs : int list) : t =
  {
    mem = Array.of_list program;
    pc = 0;
    relative_base = 0;
    state = Idle;
    inputs;
    outputs = [];
  }

(* Steps the IntCode computer, mutating the memory. *)
let step (c : t) =
  (* Parse instrcution: *)
  let instruction = c.mem.(c.pc) in
  let _ = c.pc <- c.pc + 1 in
  (* Convenience getters/setters to read memory: *)
  let params (n : int) : param_t list =
    let rec read_inner modes n =
      if n = 0 then []
      else
        let arg = c.mem.(c.pc) in
        let _ = c.pc <- c.pc + 1 in
        let mode, modes = (modes mod 10, modes / 10) in
        match mode with
        | 0 -> Position arg :: read_inner modes (n - 1)
        | 1 -> Immediate arg :: read_inner modes (n - 1)
        | 2 -> Relative arg :: read_inner modes (n - 1)
    in
    read_inner (instruction / 100) n
  in
  let read (p : param_t) : int =
    let read_mem (addr : int) : int =
      if addr >= Array.length c.mem then 0 else c.mem.(addr)
    in
    match p with
    | Position addr -> read_mem addr
    | Relative addr -> read_mem (addr + c.relative_base)
    | Immediate n -> n
  in
  let write (p : param_t) (n : int) =
    let expand_mem (addr : int) =
      if addr >= Array.length c.mem then
        let new_mem = addr + 1 - Array.length c.mem in
        c.mem <- Array.append c.mem (Array.make new_mem 0)
    in
    let addr =
      match p with
      | Position addr -> addr
      | Relative addr -> addr + c.relative_base
    in
    expand_mem addr;
    c.mem.(addr) <- n
  in
  (* Opcode implementation: *)
  match instruction mod 100 with
  | 1 (* add *) ->
      let [ a; b; addr ] = params 3 in
      write addr (read a + read b)
  | 2 (* mul *) ->
      let [ a; b; addr ] = params 3 in
      write addr (read a * read b)
  | 3 (* input *) -> (
      match c.inputs with
      | hd :: tl ->
          let [ addr ] = params 1 in
          write addr hd;
          c.inputs <- tl
      | [] ->
          c.pc <- c.pc - 1;
          c.state <- WaitingForInput;
          raise Interrupt)
  | 4 (* output *) ->
      let [ addr ] = params 1 in
      c.outputs <- c.outputs @ [ read addr ]
  | 5 (* jump-if-true *) ->
      let [ arg; pc ] = params 2 in
      if read arg != 0 then c.pc <- read pc
  | 6 (* jump-if-false *) ->
      let [ arg; pc ] = params 2 in
      if read arg = 0 then c.pc <- read pc
  | 7 (* less-than *) ->
      let [ a; b; addr ] = params 3 in
      write addr (if read a < read b then 1 else 0)
  | 8 (* equals *) ->
      let [ a; b; addr ] = params 3 in
      write addr (if read a = read b then 1 else 0)
  | 9 (* relative-base *) ->
      let [ arg ] = params 1 in
      c.relative_base <- c.relative_base + read arg
  | 99 (* halt *) ->
      c.state <- Done;
      raise Interrupt
  | op -> raise (Failure (Format.sprintf "unknown instruction: %d" op))

(* Runs the IntCode computer until an interrupt is reached. *)
let rec run (c : t) : t =
  try
    step c;
    run c
  with Interrupt -> c

(* Returns the memory at the given address. *)
let read_memory (addr : int) (c : t) : int = c.mem.(addr)

(* Returns the programs outputs. *)
let get_outputs (c : t) : int list = c.outputs

(* Adds an input to the computer. *)
let push (c : t) (i : int) = c.inputs <- c.inputs @ [ i ]

(* Pops an output from the computer, if available. *)
let pop (c : t) : int option =
  match c.outputs with
  | hd :: tl ->
      c.outputs <- tl;
      Some hd
  | [] -> None

(* Runs the given program to completion, returning its outputs. *)
let run_program (program : program_t) (inputs : int list) : int list =
  let c = init program inputs |> run in
  assert (c.state = Done);
  get_outputs c
