open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/18.txt" ||> Coord.coord_of_string

(*
 * Part 1
 *)
module Graph = struct
  type t = Coord.Set.t * coord
  type node = coord

  let neighbors ((bytes, goal) : t) (pos : node) : node list =
    let is_open c =
      Coord.in_bounds ~upper:goal c && not (Coord.Set.contains bytes c)
    in
    Coord.adjacencies pos |> List.filter is_open

  let cost _ _ _ : int = 1
  let is_done ((_, goal) : t) (pos : node) : bool = pos = goal
end

module Search = Search.Make (Graph)

let part_one ?(num_bytes = 1024) ?(goal = (70, 70)) input =
  let bytes = input |> List.to_seq |> Seq.take num_bytes |> Coord.Set.of_seq in
  Search.find_shortest_path (bytes, goal) [ (0, 0) ]

(*
 * Part 2
 *)
let rec find_blocking_coord ?(goal = (70, 70)) (coords : coord list) : Coord.t =
  match coords with
  | [] -> raise (Failure "no block found")
  | c :: cs -> (
      let bytes = Coord.Set.of_list cs in
      try
        let _ = Search.find_shortest_path (bytes, goal) [ (0, 0) ] in
        c
      with Failure _ -> (find_blocking_coord [@tailcall]) ~goal cs)

let part_two ?(goal = (70, 70)) input =
  let x, y = find_blocking_coord ~goal (List.rev input) in
  Format.sprintf "%d,%d" x y

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input (part_one >> string_of_int) part_two
