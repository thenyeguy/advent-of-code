open Utils

(*
 * Types
 *)
module StringMap = Map.Make (String)

type module_type =
  | Broadcaster
  | FlipFlip of bool
  | Conjunction of bool StringMap.t

type comm_module = { name : string; type_ : module_type; outputs : string list }
type pulse = { src : string; dest : string; value : bool }
type pulse_trace = { mutable lows : int; mutable highs : int }

let record_pulse (trace : pulse_trace) (pulse : pulse) : unit =
  if pulse.value then trace.highs <- trace.highs + 1
  else trace.lows <- trace.lows + 1

(*
 * Parse input
 *)
let parse_module (line : string) : comm_module =
  Scanf.sscanf line "%c%[a-z] -> %[a-z, ]" (fun type_ name outputs ->
      let outputs = String.split_on_char ',' outputs ||> String.trim in
      let type_ =
        match type_ with
        | 'b' -> Broadcaster
        | '%' -> FlipFlip false
        | '&' -> Conjunction StringMap.empty
      in
      let name = if type_ = Broadcaster then "broadcaster" else name in
      { name; type_; outputs })

let puzzle_input = Io.read_lines "2023/data/20.txt" ||> parse_module

(*
 * Part 1
 *)
let get_edges (m : comm_module) : (string * string) list =
  m.outputs ||> fun o -> (m.name, o)

let initialize (modules : comm_module list) : comm_module StringMap.t =
  let initalize_conjuction modules (src, dest) =
    match StringMap.find_opt dest modules with
    | Some { name; type_ = Conjunction values; outputs } ->
        let values' = StringMap.add src false values in
        StringMap.add dest
          { name; type_ = Conjunction values'; outputs }
          modules
    | _ -> modules
  in
  let edges = modules ||> get_edges |> List.flatten in
  let module_map = modules ||> (fun m -> (m.name, m)) |> StringMap.of_list in
  List.fold_left initalize_conjuction module_map edges

let handle_pulse (m : comm_module) (p : pulse) : comm_module * pulse list =
  let update t = { name = m.name; type_ = t; outputs = m.outputs } in
  let send value = m.outputs ||> fun dest -> { src = m.name; dest; value } in
  match m.type_ with
  | Broadcaster -> (m, send p.value)
  | FlipFlip value ->
      if p.value then (m, [])
      else (update (FlipFlip (not value)), send (not value))
  | Conjunction values ->
      let values' = StringMap.add p.src p.value values in
      let value = StringMap.fold (fun _ v acc -> acc && v) values' true in
      (update (Conjunction values'), send (not value))

let press_button (trace : pulse_trace) (modules : comm_module StringMap.t) :
    comm_module StringMap.t =
  let rec handle_pulses modules pulses =
    match pulses with
    | [] -> modules
    | pulse :: pulses -> (
        record_pulse trace pulse;
        match StringMap.find_opt pulse.dest modules with
        | None -> handle_pulses modules pulses
        | Some module_ ->
            let module', pulses' = handle_pulse module_ pulse in
            let modules' = StringMap.add pulse.dest module' modules in
            handle_pulses modules' (pulses @ pulses'))
  in
  handle_pulses modules
    [ { src = "button"; dest = "broadcaster"; value = false } ]

let press_buttons (times : int) (modules : comm_module list) : pulse_trace =
  let trace = { lows = 0; highs = 0 } in
  let press_button' ms _ = press_button trace ms in
  let _ =
    Seq.fold_left press_button' (initialize modules) (Seq.init times Fn.id)
  in
  trace

let part_one (input : comm_module list) : int =
  let trace = press_buttons 1000 input in
  trace.lows * trace.highs

(*
 * Part 2
 *
 * The circuit has four branches. Each branch has a starting flipflop triggered
 * by the button, and a final conjunction. The final conjunctions are anded
 * together before feeding to the `rx` node.
 *
 * Each branch enters a series of flipflips that either read from or write to
 * the conjunction, functioning as bits. The conjunction will output low when all
 * its inputs are high; then it will reset all its bits to low and repeat. This
 * will happen when its outputs are low, so we can find the cycle length by
 * converting the flipflop chains to a single binary integer.
 *
 * The entire circuit outputs when all branches are low, so we find the LCM.
 *)
let edge_map (modules : comm_module list) : string list StringMap.t =
  let unpack edges =
    let dest, _ = List.hd edges in
    let srcs = List.map Pair.right edges in
    (dest, srcs)
  in
  modules ||> get_edges ||> unpack |> StringMap.of_list

let get_chain (edges : string list StringMap.t)
    (modules : comm_module StringMap.t) (entrance : string) :
    (string * bool) list =
  let is_in_chain m =
    match StringMap.find m modules with
    | { type_ = Conjunction _; _ } -> false
    | _ -> true
  in
  let rec get_chain' chain m =
    match List.find_opt is_in_chain (StringMap.find m edges) with
    | None -> [ m ]
    | Some m' -> m :: get_chain' chain m'
  in
  let is_exit m = StringMap.find m edges |> List.exists (is_in_chain >> not) in
  get_chain' [] entrance ||> fun m -> (m, is_exit m)

let get_chains (modules : comm_module list) : (string * bool) list list =
  let edges = edge_map modules in
  let modules = initialize modules in
  StringMap.find "broadcaster" edges ||> get_chain edges modules

let get_cycle_length (chain : (string * bool) list) : int =
  let read_binary acc (_, bit) = (2 * acc) + if bit then 1 else 0 in
  chain |> List.rev |> List.fold_left read_binary 0

let lcm (l : int list) : int =
  let lcm' a b =
    let rec gcd n =
      match (a mod n, b mod n) with 0, 0 -> n | _ -> gcd (n - 1)
    in
    a * b / gcd (min a b)
  in
  List.fold_left lcm' 1 l

let part_two (input : comm_module list) : int =
  input |> get_chains ||> get_cycle_length |> lcm

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
