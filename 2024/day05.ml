open Utils
open Utils.List.Infix
open Utils.Fn.Infix
module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

(*
 * Parse input
 *)
type input_t = { page_orders : (int * int) list; updates : int list list }

let parse_input (lines : string list) : input_t =
  let parse_order o =
    let [ l; r ] = String.split_on_char '|' o in
    (int_of_string l, int_of_string r)
  in
  let parse_update = List.of_string ~sep:',' in
  let [ orders; updates ] = Io.split_blocks lines in
  {
    page_orders = List.map parse_order orders;
    updates = List.map parse_update updates;
  }

let puzzle_input = Io.read_lines "2024/data/05.txt" |> parse_input

(*
 * Part 1
 *)
(* Returns a map from page to pages that must come after. *)
let make_deps (page_orders : (int * int) list) : IntSet.t IntMap.t =
  let add m (l, r) = IntMap.add_to_list l r m in
  List.fold_left add IntMap.empty page_orders |> IntMap.map IntSet.of_list

let is_valid_update (deps : IntSet.t IntMap.t) (update : int list) : bool =
  let valid_page seen p =
    match IntMap.find_opt p deps with
    | Some ps -> IntSet.inter ps seen |> IntSet.is_empty
    | None -> true
  in
  let check (valid, seen) p =
    let valid' = valid && valid_page seen p in
    let seen' = IntSet.add p seen in
    (valid', seen')
  in
  let valid, _ = List.fold_left check (true, IntSet.empty) update in
  valid

let middle_number (update : int list) : int =
  List.nth update (List.length update / 2)

let part_one (input : input_t) =
  let deps = make_deps input.page_orders in
  input.updates
  |> List.filter (is_valid_update deps)
  ||> middle_number |> List.sum

(*
 * Part 2
 *)
let set_contains seen p = Option.is_some (IntSet.find_opt p seen)

let filter_deps (deps : IntSet.t IntMap.t) (pages : int list) :
    IntSet.t IntMap.t =
  let pages = IntSet.of_list pages in
  let f p ps =
    if set_contains pages p then Some (IntSet.inter ps pages) else None
  in
  IntMap.filter_map f deps

let reorder_pages (deps : IntSet.t IntMap.t) (pages : int list) : int list =
  (* Get a list of how many pages come after a page, then sort in descending
     order by count. *)
  let cmp (_, c1) (_, c2) = c2 - c1 in
  filter_deps deps pages |> IntMap.map IntSet.cardinal |> IntMap.to_list
  |> List.sort cmp ||> Pair.left

let part_two (input : input_t) =
  let deps = make_deps input.page_orders in
  input.updates
  |> List.filter (is_valid_update deps >> not)
  ||> reorder_pages deps ||> middle_number |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
