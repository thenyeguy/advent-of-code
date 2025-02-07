open Utils

(*
 * Parse input
 *)
type technique = Reverse | Cut of int | Deal of int

let parse (line : string) : technique =
  if line = "deal into new stack" then Reverse
  else
    let n = String.split_on_char ' ' line |> List.last |> int_of_string in
    if String.starts_with ~prefix:"cut" line then Cut n else Deal n

let puzzle_input = Io.read_lines "2019/data/22.txt" ||> parse

(*
 * Part 1
 *)
let rec cut (deck : int list) (cards : int) : int list =
  let open Seq in
  if cards < 0 then cut deck (List.length deck + cards)
  else
    let s = List.to_seq deck in
    append (drop cards s) (take cards s) |> List.of_seq

let deal (deck : int list) (inc : int) : int list =
  let len = List.length deck in
  let a = Array.make len 0 in
  let rec deal_inner ds i =
    match ds with
    | [] -> ()
    | d :: ds ->
        a.(i) <- d;
        deal_inner ds ((i + inc) mod len)
  in
  deal_inner deck 0;
  Array.to_list a

let apply (deck : int list) (t : technique) : int list =
  match t with
  | Reverse -> List.rev deck
  | Cut cards -> cut deck cards
  | Deal inc -> deal deck inc

let shuffle (deck : int list) (ts : technique list) : int list =
  List.fold_left apply deck ts

let factory_deck (cards : int) = List.init cards Fn.id

let part_one input =
  let deck = factory_deck 10007 in
  shuffle deck input |> List.find_index (( = ) 2019) |> Option.get

(*
 * Part 2
 *)
let part_two = Runner.unimplemented

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
