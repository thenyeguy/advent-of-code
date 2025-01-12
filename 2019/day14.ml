open Utils

(*
 * Parse input
 *)
type reagent_t = int * string
type reaction_t = { inputs : reagent_t list; output : reagent_t }

let parse (line : string) : reaction_t =
  let [ ins; outs ] = String.split_on_char '=' line in
  let output = Scanf.sscanf outs "> %d %s" Pair.pack in
  let inputs =
    String.split_on_char ',' ins ||> fun s ->
    Scanf.sscanf (String.trim s) "%d %s" Pair.pack
  in
  { inputs; output }

let puzzle_input = Io.read_lines "2019/data/14.txt" ||> parse

(*
 * Part 1
 *)
module SMap = Map.Make (String)

(* Division that rounds up. *)
let ( ^/ ) l r =
  assert (l > 0);
  ((l - 1) / r) + 1

let make_recipes (rs : reaction_t list) : (int * reagent_t list) SMap.t =
  let acc m r =
    let quantity, output = r.output in
    SMap.add output (quantity, r.inputs) m
  in
  List.fold_left acc SMap.empty rs

let produce (rs : reaction_t list) ?(count : int = 1) (output : string) :
    int SMap.t =
  let deps = make_recipes rs in
  let rec make produced remaining count output =
    if output = "ORE" then (SMap.increment produced ~count "ORE", remaining)
    else
      (* Look up the recipe. *)
      let available = SMap.find_or output 0 remaining in
      if count <= available then
        (* If we have enough leftover, no need to produce more. *)
        (produced, SMap.decrement remaining ~count output)
      else
        (* Scale the recipe to make enough. *)
        let recipe_out, reagents = SMap.find output deps in
        let scale = (count - SMap.find_or output 0 remaining) ^/ recipe_out in
        let produced =
          SMap.increment produced ~count:(scale * recipe_out) output
        in
        let remaining =
          SMap.increment remaining ~count:((scale * recipe_out) - count) output
        in
        (* Recursively make the reagents. *)
        let make_reagents (produced, remaining) (count, reagent) =
          make produced remaining (scale * count) reagent
        in
        List.fold_left make_reagents (produced, remaining) reagents
  in
  make SMap.empty SMap.empty count output |> fst

let count_ore ?(count : int = 1) (reagents : reaction_t list) =
  produce reagents ~count "FUEL" |> SMap.find "ORE"

let part_one = count_ore

(*
 * Part 2
 *)
let part_two input =
  let max_ore = 1000000000000 in
  let max_fuel = max_ore / part_one input in
  let can_produce count = count_ore ~count input <= max_ore in
  Search.binary_search ~lower:max_fuel ~upper:(2 * max_fuel) can_produce

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
