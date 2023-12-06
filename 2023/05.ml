(*
 * Utility function
 *)
let read_file (fname : string) : string list =
  let rec read_lines (ch : in_channel) : string list =
    try
      let line = input_line ch in
      line :: read_lines ch
    with End_of_file -> []
  in
  let ic = open_in fname in
  let lines = read_lines ic in
  close_in ic;
  lines

let id (x : 'a) : 'a = x

let rec evens (ls : 'a list) : 'a list =
  match ls with a :: b :: tail -> a :: evens tail | _ -> []

let rec odds (ls : 'a list) : 'a list =
  match ls with a :: b :: tail -> b :: odds tail | _ -> []

(*
 * Input parsing
 *)
type range = { start : int; rlength : int }
type mapping = { src : int; dest : int; mlength : int }
type almanac = { seeds : int list; maps : mapping list list }

let split_blocks (input : string list) : string list list =
  let accumulate block line =
    match line with
    | "" -> ([], Option.some block)
    | _ -> (List.append block [ line ], Option.none)
  in
  let block, blocks = List.fold_left_map accumulate [] input in
  List.append (List.filter_map id blocks) [ block ]

let parse_numbers (s : string) : int list =
  String.split_on_char ' ' s |> List.filter_map int_of_string_opt

let parse_input (input : string list) : almanac =
  let [ _; seeds_str ] = input |> List.hd |> String.split_on_char ':' in
  let blocks = input |> split_blocks |> List.tl in
  let parse_block (block : string list) : mapping list =
    let parse_line line =
      let [ d; s; l ] = parse_numbers line in
      { src = s; dest = d; mlength = l }
    in
    block |> List.tl |> List.map parse_line
  in
  { seeds = parse_numbers seeds_str; maps = List.map parse_block blocks }

let puzzle_input () = parse_input (read_file "05.txt")

(*
 * Part 1
 *)
let map_value (n : int) (ms : mapping list) : int =
  let _map (n : int) (m : mapping) : int option =
    let offset = n - m.src in
    if 0 <= offset && offset < m.mlength then Option.some (m.dest + offset)
    else Option.none
  in
  match List.filter_map (_map n) ms with [ nn ] -> nn | _ -> n

let get_location (a : almanac) (seed : int) : int =
  List.fold_left map_value seed a.maps

let part_one (a : almanac) =
  a.seeds |> List.map (get_location a) |> List.fold_left min max_int

(*
 * Part 2
 *)
let split_range (m : mapping) (r : range) : range option list * range option =
  let ms, me, rs, re =
    (m.src, m.src + m.mlength, r.start, r.start + r.rlength)
  in
  let left =
    if rs < ms then Option.some { start = rs; rlength = min re ms - rs }
    else Option.none
  in
  let middle =
    if rs < me && ms < re then
      let s, e = (max rs ms, min re me) in
      let offset = s - ms in
      Option.some { start = m.dest + offset; rlength = e - s }
    else Option.none
  in
  let right =
    if me < re then Option.some { start = max rs me; rlength = re - max rs me }
    else Option.none
  in
  ([ left; right ], middle)

let map_ranges (rs : range list) (ms : mapping list) : range list =
  let accumulate acc m =
    let rem, mapped = acc |> List.map (split_range m) |> List.split in
    (rem |> List.flatten |> List.filter_map id, mapped)
  in
  let acc, mapped = List.fold_left_map accumulate rs ms in
  List.append (mapped |> List.flatten |> List.filter_map id) acc

let get_locations (ms : mapping list list) (rs : range list) : range list =
  List.fold_left map_ranges rs ms

let part_two (a : almanac) =
  let range_of s l = { start = s; rlength = l } in
  let ranges = List.map2 range_of (evens a.seeds) (odds a.seeds) in
  let mapped_ranges = get_locations a.maps ranges in
  mapped_ranges |> List.map (fun r -> r.start) |> List.fold_left min max_int
