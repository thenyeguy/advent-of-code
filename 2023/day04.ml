open Utils
open Utils.List.Infix
module IntSet = Set.Make (Int)

type card = { winning : int list; numbers : int list }

let parse_numbers (s : string) : int list =
  String.split_on_char ' ' s |> List.filter_map int_of_string_opt

let parse_card (line : string) : card =
  let [ _; rest ] = String.split_on_char ':' line in
  let [ w; n ] = String.split_on_char '|' rest in
  { winning = parse_numbers w; numbers = parse_numbers n }

let count_winning_numbers (c : card) : int =
  let ws = IntSet.of_list c.winning in
  let ns = IntSet.of_list c.numbers in
  IntSet.inter ws ns |> IntSet.cardinal

let score_card (c : card) : int =
  let rec score n = match n with 0 -> 0 | 1 -> 1 | _ -> 2 * score (n - 1) in
  count_winning_numbers c |> score

let score_all_cards (cards : card list) : int =
  let scores = List.map count_winning_numbers cards in
  let rec list_add left right =
    match (left, right) with
    | [], _ -> right
    | _, [] -> left
    | l :: ls, r :: rs -> (l + r) :: list_add ls rs
  in
  (* val fold_left_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list *)
  let accumulate (acc : int list) (winners : int) : int list * int =
    let (copies :: rest) = acc in
    let new_cards = List.repeated copies winners in
    (list_add rest new_cards, copies)
  in
  let acc = List.repeated 1 (List.length cards) in
  let _, card_counts = List.fold_left_map accumulate acc scores in
  List.sum card_counts

let puzzle_input = Io.read_lines "2023/data/04.txt" ||> parse_card
let part_one (cards : card list) = cards ||> score_card |> List.sum
let part_two (cards : card list) = score_all_cards cards
let _ = Runner.main puzzle_input part_one part_two
