open Utils
open Utils.List.Infix

(*
 * Input parsing
 *)
type card = int
type hand = card list

let parse_card (c : char) : card =
  match c with
  | '1' .. '9' -> Char.code c - Char.code '0'
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14

let parse_hand (s : string) : hand =
  String.to_seq s |> Seq.map parse_card |> List.of_seq

let parse_bet (s : string) : hand * int =
  let [ h; b ] = String.split_on_char ' ' s in
  (parse_hand h, int_of_string b)

let puzzle_input = Io.read_lines "data/07.txt" ||> parse_bet

(*
 * Part 1
 *)
type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard

let compare_hand_types (l : hand_type) (r : hand_type) : int =
  let int_of_hand_type (t : hand_type) : int =
    match t with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
  in
  compare (int_of_hand_type l) (int_of_hand_type r)

let count_cards (h : hand) : (card * int) list =
  let counts = Hashtbl.create 5 in
  let count_card c =
    let count = Hashtbl.find_opt counts c |> Option.value ~default:0 in
    Hashtbl.replace counts c (count + 1)
  in
  let compare_counts (lcard, lcount) (rcard, rcount) =
    match (compare lcount rcount, compare lcard rcard) with
    | 0, c -> c
    | c, _ -> c
  in
  List.iter count_card h;
  Hashtbl.to_seq counts |> List.of_seq |> List.sort compare_counts |> List.rev

let classify_hand (h : hand) : hand_type =
  match List.map Pair.right (count_cards h) with
  | [ 5 ] -> FiveOfAKind
  | 4 :: _ -> FourOfAKind
  | [ 3; 2 ] -> FullHouse
  | 3 :: _ -> ThreeOfAKind
  | 2 :: 2 :: _ -> TwoPair
  | 2 :: _ -> OnePair
  | _ -> HighCard

let compare_card_strength left right =
  let accumulate acc l r = if acc = 0 then compare l r else acc in
  List.fold_left2 accumulate 0 left right

let compare_hands (left : hand) (right : hand) : int =
  match compare_hand_types (classify_hand left) (classify_hand right) with
  | 0 -> compare_card_strength left right
  | c -> c

let compute_winnings (cmp : hand -> hand -> int) (bets : (hand * int) list) :
    int =
  let compare_bets (lhand, _) (rhand, _) = cmp lhand rhand in
  let score_hand i (_, bet) = (i + 1) * bet in
  bets |> List.sort compare_bets |> List.mapi score_hand
  |> List.fold_left ( + ) 0

let part_one (input : (hand * int) list) = compute_winnings compare_hands input

(*
 * Part 2
 *)
let joker = 11

let jokerfy (h : hand) (value : int) : hand =
  let jokerfy' c = if c == joker then value else c in
  List.map jokerfy' h

let best_card (h : hand) : card =
  let h' = List.filter (fun c -> c != joker) h in
  let counts = count_cards h' in
  match counts with (c, _) :: _ -> c | _ -> joker

let compare_joker_hands (left : hand) (right : hand) : int =
  let left_type = best_card left |> jokerfy left |> classify_hand in
  let right_type = best_card right |> jokerfy right |> classify_hand in
  match compare_hand_types left_type right_type with
  | 0 -> compare_card_strength (jokerfy left 0) (jokerfy right 0)
  | c -> c

let part_two (input : (hand * int) list) =
  compute_winnings compare_joker_hands input

let _ = Runner.main puzzle_input part_one part_two
