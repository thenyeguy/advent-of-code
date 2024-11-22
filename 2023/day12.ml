open Utils
open Utils.List.Infix

(*
 * Types
 *)
type row = { springs : char list; runs : int list }

type wip_run = {
  (* Number of expected '#' remaining, if we are in a run. *)
  current : int option;
  (* The remaining runs we haven't seen. *)
  remaining : int list;
  (* How many copies of this state we've seen. *)
  count : int;
}

let compare_wips (left : wip_run) (right : wip_run) =
  let current = compare left.current right.current in
  let remaining = compare left.remaining right.remaining in
  if current = 0 then remaining else current

let wips_equal (left : wip_run) (right : wip_run) : bool =
  compare_wips left right = 0

(*
 * Parse input
 *)
let parse_row (line : string) : row =
  let [ springs; runs ] = String.split_on_char ' ' line in
  let springs = String.explode springs in
  let runs = List.of_string ~sep:',' runs in
  { springs; runs }

let puzzle_input () = Io.read_lines "2023/data/12.txt" ||> parse_row

(*
 * Part 1
 *)
let count_valid_springs (row : row) : int =
  (* Appends the provided character to the run, if valid. *)
  let append (next_spring : char) (wip : wip_run) : wip_run option =
    match (next_spring, wip) with
    | '#', { current = None; remaining = []; _ } -> Option.none
    | '#', { current = None; remaining = next :: remaining; count } ->
        Option.some { current = Option.some (next - 1); remaining; count }
    | '#', { current = Some 0; _ } -> Option.none
    | '#', { current = Some c; remaining; count } ->
        Option.some { current = Option.some (c - 1); remaining; count }
    | '.', { current = None; _ } -> Option.some wip
    | '.', { current = Some 0; remaining; count } ->
        Option.some { current = Option.none; remaining; count }
    | '.', { current = Some _; _ } -> Option.none
  in
  (* Combines a series of identical runs by summing their counts. *)
  let collapse (wips : wip_run Seq.t) : wip_run =
    let count = Seq.fold_left (fun acc wip -> acc + wip.count) 0 wips in
    let (Some (wip, _)) = Seq.uncons wips in
    { current = wip.current; remaining = wip.remaining; count }
  in
  (* Given a list with duplicates, counts and dedupes the runs. *)
  let merge (wip : wip_run list) : wip_run list =
    wip |> List.sort compare_wips |> List.to_seq |> Seq.group wips_equal
    |> Seq.map collapse |> List.of_seq
  in
  (* Adds a single character to all the provided runs. *)
  let rec step (wips : wip_run list) (next : char) : wip_run list =
    let wips' =
      match next with
      | '?' -> step wips '#' @ step wips '.'
      | _ -> List.filter_map (append next) wips
    in
    merge wips'
  in
  (* Adds this runs count, if it is complete. *)
  let count_complete (acc : int) (wip : wip_run) : int =
    match (wip.current, wip.remaining) with
    | None, [] -> acc + wip.count
    | Some 0, [] -> acc + wip.count
    | _ -> acc
  in
  let start = { current = Option.none; remaining = row.runs; count = 1 } in
  List.fold_left step [ start ] row.springs |> List.fold_left count_complete 0

let part_one (input : row list) = input ||> count_valid_springs |> List.sum

(*
 * Part 2
 *)
let expand_row (r : row) : row =
  let springs' = '?' :: r.springs in
  {
    springs = r.springs @ springs' @ springs' @ springs' @ springs';
    runs = r.runs @ r.runs @ r.runs @ r.runs @ r.runs;
  }

let part_two (input : row list) =
  input ||> expand_row ||> count_valid_springs |> List.sum

(*
 * Main
 *)
let _ = Runner.main (puzzle_input ()) part_one part_two
