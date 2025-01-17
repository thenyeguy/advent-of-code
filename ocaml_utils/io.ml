(* Reads a file into a string. *)
let read_file (fname : string) : string =
  let ic = open_in fname in
  let s = In_channel.input_all ic in
  close_in ic;
  s

(* Reads a file into a list line-by-line. *)
let read_lines (fname : string) : string list =
  let rec read_lines' (ch : in_channel) : string list =
    try
      let line = input_line ch in
      line :: read_lines' ch
    with End_of_file -> []
  in
  let ic = open_in fname in
  let lines = read_lines' ic in
  close_in ic;
  lines

(* Splits a list of lines into blocks, seperated by empty lines. *)
let split_blocks (input : string list) : string list list =
  let accumulate block line =
    match line with
    | "" -> ([], Some block)
    | _ -> (List.append block [ line ], None)
  in
  let block, blocks = List.fold_left_map accumulate [] input in
  List.append (List.filter_none blocks) [ block ]
