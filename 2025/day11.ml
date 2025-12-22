open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/11.txt"

(*
 * Part 1
 *)
module StringMap = Map.Make (String)

let get_edges (lines : string list) : string list StringMap.t =
  let add_edges map line =
    let (src :: dsts) = String.words line in
    let src = String.sub src 0 3 in
    let add_edge map dst = StringMap.add_to_list src dst map in
    List.fold_left add_edge map dsts
  in
  List.fold_left add_edges StringMap.empty lines

let count_paths ?(src : string = "you") ?(dst : string = "out")
    (edges : string list StringMap.t) : int =
  let search self pos =
    if pos = dst then 1
    else
      match StringMap.find_opt pos edges with
      | None -> 0
      | Some frontier -> List.map self frontier |> List.sum
  in
  Memo.memo_rec search src

let part_one = get_edges >> count_paths

(*
 * Part 2
 *)
let count_valid_paths (edges : string list StringMap.t) : int =
  count_paths ~src:"svr" ~dst:"fft" edges
  * count_paths ~src:"fft" ~dst:"dac" edges
  * count_paths ~src:"dac" ~dst:"out" edges

let part_two = get_edges >> count_valid_paths

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
