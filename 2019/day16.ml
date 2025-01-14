open Utils

(*
 * Parse input
 *)
let puzzle_input =
  Io.read_file "2019/data/16.txt" |> String.explode ||> Char.to_digit

(*
 * Part 1
 *)
let make_pattern (i : int) : int Seq.t =
  (* 0, 1, 0, -1 *)
  let repeated n = List.repeated n (i + 1) in
  List.concat_map repeated [ 0; 1; 0; -1 ]
  |> List.to_seq |> Seq.cycle |> Seq.drop 1

let make_patterns (n : int) : int list list =
  List.range n ||> make_pattern ||> Seq.take n ||> List.of_seq

let apply_patterns (patterns : int list list) (signal : int list) : int list =
  let apply pattern =
    let out = List.map2 ( * ) signal pattern |> List.sum in
    out mod 10 |> abs
  in
  List.map apply patterns

let fft ?(phases : int = 100) (signal : int list) : int list =
  let patterns = make_patterns (List.length signal) in
  Fn.repeat (apply_patterns patterns) phases signal

let take ?(digits : int = 8) (signal : int list) : int =
  let acc total i = (10 * total) + i in
  List.to_seq signal |> Seq.take digits |> Seq.fold_left acc 0

let part_one input = fft input |> take

(*
 * Part 2
 *)
let fast_fft ?(phases : int = 100) (offset : int) (length : int)
    (base_signal : int list) : int list =
  assert (offset > length / 2);
  let apply signal =
    let acc sum i =
      let sum = (sum + i) mod 10 in
      (sum, sum)
    in
    List.fold_left_map acc 0 signal |> snd
  in
  let signal =
    base_signal |> List.rev |> List.to_seq |> Seq.cycle
    |> Seq.take (length - offset)
    |> List.of_seq
  in
  Fn.repeat apply phases signal |> List.rev

let rec expand_signal (times : int) (signal : int list) : int list =
  if times = 0 then signal else signal @ expand_signal (times - 1) signal

let part_two input =
  let offset = take ~digits:7 input in
  fast_fft ~phases:100 offset (10000 * List.length input) input |> take

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
