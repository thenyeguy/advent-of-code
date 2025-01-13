open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/03.txt" |> String.concat ""

(*
 * Part 1
 *)
type parse_state = Seeking | Left of int | Right of (int * int)
type command = Mul of (int * int) | Do | Dont

let print_state s =
  match s with
  | Seeking -> print_endline "Seeking"
  | Left l ->
      print_string "Left ";
      print_int l;
      print_newline ()
  | Right (l, r) ->
      print_string "Right ";
      print_int l;
      print_string " ";
      print_int r;
      print_newline ()

let parse (input : string) : command list =
  let rec parse' (cs : char list) (state : parse_state) : command list =
    match state with
    | Seeking -> (
        match cs with
        | [] -> []
        | 'm' :: 'u' :: 'l' :: '(' :: cs' -> parse' cs' (Left 0)
        | 'd' :: 'o' :: '(' :: ')' :: cs' -> Do :: parse' cs' Seeking
        | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: cs' ->
            Dont :: parse' cs' Seeking
        | _ :: cs' -> parse' cs' Seeking)
    | Left l -> (
        match cs with
        | ',' :: cs' -> parse' cs' (Right (l, 0))
        | ('0' .. '9' as c) :: cs' ->
            let l' = (10 * l) + Char.digit_of_char c in
            parse' cs' (Left l')
        | _ -> parse' cs Seeking)
    | Right (l, r) -> (
        match cs with
        | ')' :: cs' -> Mul (l, r) :: parse' cs' Seeking
        | ('0' .. '9' as c) :: cs' ->
            let r' = (10 * r) + Char.digit_of_char c in
            parse' cs' (Right (l, r'))
        | _ -> parse' cs Seeking)
  in
  parse' (String.explode input) Seeking

let eval_command (c : command) : int =
  match c with Mul (l, r) -> l * r | _ -> 0

let part_one input = input |> parse ||> eval_command |> List.sum

(*
 * Part 2
 *)
let eval (cs : command list) : int =
  let acc (d, n) c =
    match c with
    | Do -> (true, n)
    | Dont -> (false, n)
    | Mul (l, r) ->
        let n' = if d then n + (l * r) else n in
        (d, n')
  in
  List.fold_left acc (true, 0) cs |> Pair.right

let part_two input = input |> parse |> eval

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
