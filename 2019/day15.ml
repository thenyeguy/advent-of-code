open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/15.txt"

(*
 * Part 1
 *)
module Graph = struct
  type t = coord * Coord.Set.t
  type node = coord

  let neighbors ((_, reachable) : t) (pos : node) : node list =
    Coord.adjacencies pos |> List.filter (Coord.Set.contains reachable)

  let cost _ _ _ = 1
  let is_done ((goal, _) : t) (pos : node) : bool = pos = goal
end

module Search = Search.Make (Graph)

let int_of_dir (dir : dir) : int =
  let open Coord in
  match dir with Up -> 1 | Down -> 2 | Left -> 3 | Right -> 4

let explore (prog : Intcode.Computer.program_t) : Graph.t =
  let open Intcode.Computer in
  let oxygen = ref (0, 0) in
  let c = init prog in
  (* Send a move command to the robot and return its output. *)
  let move (dir : dir) : int =
    push c (int_of_dir dir);
    ignore (run c);
    pop c
  in
  let rec dfs (seen : Coord.Set.t) (pos : coord) =
    let seen = Coord.Set.add pos seen in
    let try_dir seen dir =
      let pos = Coord.step pos dir in
      if Coord.Set.contains seen pos then seen
      else
        match move dir with
        | 0 -> seen
        | (1 | 2) as result ->
            if result = 2 then oxygen := pos;
            let seen = dfs seen pos in
            (* Step back before moving on. *)
            Coord.flip dir |> move |> ignore;
            seen
    in
    List.fold_left try_dir seen Coord.[ Up; Left; Down; Right ]
  in
  let reachable = dfs Coord.Set.empty (0, 0) in
  (!oxygen, reachable)

let shortest_path (prog : Intcode.Computer.program_t) : int =
  let graph = explore prog in
  Search.find_shortest_path graph [ (0, 0) ]

let part_one = shortest_path

(*
 * Part 2
 *)
let time_oxygen (prog : Intcode.Computer.program_t) : int =
  let ((goal, _) as graph) = explore prog in
  let times = Search.find_distance_from graph [ goal ] in
  Search.NodeMap.to_list times ||> snd |> List.max

let part_two = time_oxygen

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
