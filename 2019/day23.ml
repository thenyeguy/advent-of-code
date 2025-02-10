open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/23.txt"

(*
 * Part 1
 *)
type packet = { id : int; x : int; y : int }

type network = {
  cs : Intcode.Computer.t array;
  bus : packet Queue.t array;
  mutable out : packet option;
}

let initialize (prog : Intcode.Computer.program_t) : network =
  let open Intcode.Computer in
  let cs = Array.init 50 (fun id -> init ~inputs:[ id ] prog) in
  let bus = Array.init 50 (fun _ -> Queue.create ()) in
  { cs; bus; out = None }

let read_packet (c : Intcode.Computer.t) : packet =
  let open Intcode.Computer in
  let id = pop c in
  let x = pop c in
  let y = pop c in
  { id; x; y }

let tick_network (n : network) =
  let open Intcode.Computer in
  let tick c q =
    (if Queue.is_empty q then push c (-1)
     else
       let p = Queue.pop q in
       push c p.x;
       push c p.y);
    ignore (run c);
    while has_output c do
      let p = read_packet c in
      if p.id = 255 then n.out <- Some p else Queue.push p n.bus.(p.id)
    done
  in
  Array.iter2 tick n.cs n.bus

let get_output (n : network) : packet =
  while n.out = None do
    tick_network n
  done;
  Option.get n.out

let part_one input =
  let p = initialize input |> get_output in
  p.y

(*
 * Part 2
 *)
let run_until_idle (n : network) : packet =
  let is_idle n =
    let acc all q = all && Queue.is_empty q in
    let o = Array.fold_left acc true n.bus in
    o
  in
  tick_network n;
  while not (is_idle n) do
    tick_network n
  done;
  Option.get n.out

let monitor (n : network) : int =
  let last_y = ref 0 in
  let rec tick () =
    let p = run_until_idle n in
    if !last_y = p.y then p.y
    else (
      last_y := p.y;
      Queue.push p n.bus.(0);
      tick ())
  in
  tick ()

let part_two = initialize >> monitor

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
