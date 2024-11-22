open Utils

let is_digit (c : char) : bool = '0' <= c && c <= '9'

let get_digits (s : string) : char list =
  s |> String.explode |> List.filter is_digit

let next_digit (chars : char list) : char option * char list =
  match chars with
  | [] -> (None, [])
  | 'o' :: 'n' :: 'e' :: _ -> (Some '1', List.tl chars)
  | 't' :: 'w' :: 'o' :: _ -> (Some '2', List.tl chars)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: _ -> (Some '3', List.tl chars)
  | 'f' :: 'o' :: 'u' :: 'r' :: _ -> (Some '4', List.tl chars)
  | 'f' :: 'i' :: 'v' :: 'e' :: _ -> (Some '5', List.tl chars)
  | 's' :: 'i' :: 'x' :: _ -> (Some '6', List.tl chars)
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: _ -> (Some '7', List.tl chars)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: _ -> (Some '8', List.tl chars)
  | 'n' :: 'i' :: 'n' :: 'e' :: _ -> (Some '9', List.tl chars)
  | c :: cs when is_digit c -> (Some c, cs)
  | _ :: cs -> (None, cs)

let get_named_digits (s : string) : char list =
  let rec scan (l : char list) =
    match next_digit l with
    | None, [] -> []
    | None, cs -> scan cs
    | Some c, cs -> c :: scan cs
  in
  s |> String.explode |> scan

let calibration_value (f : string -> char list) (s : string) : int =
  let ds = f s in
  let first = List.hd ds in
  let last = List.last ds in
  int_of_string (Char.escaped first ^ Char.escaped last)

let test_input_one = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ]

let test_input_two =
  [
    "two1nine";
    "eightwothree";
    "abcone2threexyz";
    "xtwone3four";
    "4nineeightseven2";
    "zoneight234";
    "7pqrstsixteen";
  ]

let puzzle_input : string list = Io.read_lines "2023/data/01.txt"

let part_one input =
  let values = List.map (calibration_value get_digits) input in
  List.fold_left ( + ) 0 values

let part_two input =
  let values = List.map (calibration_value get_named_digits) input in
  List.fold_left ( + ) 0 values

let _ = Runner.main puzzle_input part_one part_two
