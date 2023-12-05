let read_file (fname: string): ('a list) =
    let rec read_lines (ch : in_channel) : (string list) =
        try
            let line = input_line ch in
            line :: read_lines ch
        with
        | End_of_file -> [] in
    let ic = open_in fname in
    let lines = read_lines ic in
    close_in ic;
    lines

let id x = x
let idx i x = (i, x)
let (||>) f g x = g (f x)
let explode_string: (string -> char list) = String.to_seq ||> List.of_seq

type coord = int*int
type symbol = { sym_pos: coord; sym: char }
type part = { part_pos: coord list; num: int }

let adjacent_coords (row, col: coord): coord list =
    [
        (row-1, col-1); (row-1, col); (row-1, col+1);
        (row, col-1);                 (row, col+1);
        (row+1, col-1); (row+1, col); (row+1, col+1)
    ]

let symbols_in_row (rowi: int) (row: string): symbol list =
    let symbol_idx i c =
        match c with
            '0'..'9' -> Option.none
          | '.' -> Option.none
          | _ -> Option.some { sym_pos=(rowi, i); sym=c }
    in
    explode_string row |> List.mapi symbol_idx |> List.filter_map id

let symbols_in_schematic (schematic: string list): symbol list =
    List.mapi symbols_in_row schematic |> List.flatten

let parts_in_row (rowi: int) (row: string): part list =
    let parse_digit d = (Char.code d) - (Char.code '0') in
    let add_to_part part coli digit =
        { part_pos=(rowi,coli)::part.part_pos; num=(10*part.num + (parse_digit digit)) }
    in
    let accumulate_parts acc (coli,c) =
        match (acc, c) with
            (None, '0'..'9') ->
                (Option.some { part_pos=[(rowi,coli)]; num=(parse_digit c) }, Option.none)
          | (None, _) -> (Option.none, Option.none)
          | (Some p, '0'..'9') -> (Option.some (add_to_part p coli c), Option.none)
          | (Some p, _) -> (Option.none, Option.some p)
    in
    let digits = explode_string row |> List.mapi idx in
    let acc, parts = digits |> List.fold_left_map accumulate_parts Option.none in
    let parts = List.filter_map id parts in
    match acc with
        Some p -> p::parts
      | None -> parts

let parts_in_schematic (schematic: string list): part list =
    List.mapi parts_in_row schematic |> List.flatten

let has_adjacent_symbol (symbols: symbol list) (part: part): bool =
    let symbol_coords = List.map (fun s -> s.sym_pos) symbols in
    let is_symbol c = List.mem c symbol_coords in
    List.map adjacent_coords part.part_pos |> List.flatten |> List.exists is_symbol

let adjacent_parts (parts: part list) (symbol: symbol): part list =
    let cs = adjacent_coords symbol.sym_pos in
    let is_adjacent c = List.mem c cs in
    let is_adjacent_part p = List.exists is_adjacent p.part_pos in
    List.filter is_adjacent_part parts

let gear_ratio (parts: part list) (symbol: symbol): int option =
    match symbol.sym, (adjacent_parts parts symbol) with
        '*', p1::p2::_ -> Option.some (p1.num * p2.num)
      | _ -> Option.none

let puzzle_input () = read_file "03.txt"

let part_one (input: string list) =
    let symbols = symbols_in_schematic input in
    let parts = parts_in_schematic input in
    List.filter (has_adjacent_symbol symbols) parts
     |> List.map (fun p -> p.num)
     |> List.fold_left (+) 0

let part_two (input: string list) =
    let symbols = symbols_in_schematic input in
    let parts = parts_in_schematic input in
    List.filter_map (gear_ratio parts) symbols |> List.fold_left (+) 0
