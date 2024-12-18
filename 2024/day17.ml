open Utils
open Utils.Fn.Infix

(*
 * Parse input
 *)
type state_t = { a : int; b : int; c : int; pc : int }
type program_t = int array

let parse (lines : string list) : program_t * state_t =
  let a = Scanf.sscanf (List.nth lines 0) "Register A: %d" Fn.id in
  let b = Scanf.sscanf (List.nth lines 1) "Register B: %d" Fn.id in
  let c = Scanf.sscanf (List.nth lines 2) "Register C: %d" Fn.id in
  let program_str = String.split_on_char ' ' (List.nth lines 4) in
  let program =
    List.nth program_str 1 |> List.of_string ~sep:',' |> Array.of_list
  in
  (program, { a; b; c; pc = 0 })

let puzzle_input = Io.read_lines "2024/data/17.txt" |> parse

(*
 * Part 1
 *)
let get_combo (state : state_t) (c : int) : int =
  match c with
  | 0 | 1 | 2 | 3 -> c
  | 4 -> state.a
  | 5 -> state.b
  | 6 -> state.c
  | _ -> raise (Failure "invalid combo")

let handle_opcode (program : program_t) (state : state_t) : state_t * int option
    =
  let opcode, literal = (program.(state.pc), program.(state.pc + 1)) in
  let combo = get_combo state literal in
  match opcode with
  | 0 (* adv *) ->
      let a = state.a / Int.shift_left 1 combo in
      ({ a; b = state.b; c = state.c; pc = state.pc + 2 }, None)
  | 1 (* bxl *) ->
      let b = Int.logxor state.b literal in
      ({ a = state.a; b; c = state.c; pc = state.pc + 2 }, None)
  | 2 (* bst *) ->
      let b = combo mod 8 in
      ({ a = state.a; b; c = state.c; pc = state.pc + 2 }, None)
  | 3 (* jnz *) ->
      if state.a = 0 then
        ({ a = state.a; b = state.b; c = state.c; pc = state.pc + 2 }, None)
      else ({ a = state.a; b = state.b; c = state.c; pc = literal }, None)
  | 4 (* bxc *) ->
      let b = Int.logxor state.b state.c in
      ({ a = state.a; b; c = state.c; pc = state.pc + 2 }, None)
  | 5 (* out *) ->
      ( { a = state.a; b = state.b; c = state.c; pc = state.pc + 2 },
        Some (combo mod 8) )
  | 6 (* bdv *) ->
      let b = Int.shift_right state.a combo in
      ({ a = state.a; b; c = state.c; pc = state.pc + 2 }, None)
  | 7 (* cdv *) ->
      let c = Int.shift_right state.a combo in
      ({ a = state.a; b = state.b; c; pc = state.pc + 2 }, None)
  | _ -> raise (Failure "invalid opcode")

let run_program (program : program_t) (initial_state : state_t) : int list =
  let rec run_program_inner (state : state_t) (output : int list) : int list =
    (* print_int state.pc; *)
    (* print_newline (); *)
    if state.pc >= Array.length program then output
    else
      let state', out = handle_opcode program state in
      let output' = match out with Some o -> o :: output | _ -> output in
      (run_program_inner [@tailcall]) state' output'
  in
  run_program_inner initial_state [] |> List.rev

let part_one =
  Pair.apply run_program >> List.map string_of_int >> String.concat ","

(*
 * Part 2
 *
 * Reverse engineered:
 *   bst a    ; b = a mod 8
 *   bxl 5    ; b = b ^ 0b101    ; b = (a mod 8) ^ 0b101
 *   cdv b    ; c = a >> b
 *   bxc _c   ; b = b ^ c        ; b' = b ^ (a >> b)
 *   adv 3    ; a = a >> 3
 *   bxl 6    ; b = b ^ 0b110    ; out = b' ^ 0b110
 *   out b    ; return b
 *   jnz 0
 * Process A in batches of 3 bits. Each batch decides the _next_ a.
 *)
let step (a : int) : int =
  let b = Int.logxor (a mod 8) 5 in
  let b' = Int.logxor b (Int.shift_right a b) in
  Int.logxor b' 6 mod 8

let compute_a (program : program_t) : int =
  let rec compute_a_inner (outputs : int list) (a : int) : int option =
    match outputs with
    | [] -> Some a
    | out :: outs ->
        (* Compute each set of 3 bits that works so far, then recurse.
         * For each valid solution, take the smallest. *)
        let as_ =
          List.range 8
          |> List.map (( + ) (Int.shift_left a 3))
          |> List.filter (fun a' -> step a' = out)
          |> List.filter_map (compute_a_inner outs)
          |> List.sort compare
        in
        List.nth_opt as_ 0
  in
  let outputs = program |> Array.to_list |> List.rev in
  compute_a_inner outputs 0 |> Option.get

let part_two (program, _) = compute_a program

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input part_one (part_two >> string_of_int)
