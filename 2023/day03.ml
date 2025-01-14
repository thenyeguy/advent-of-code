open Utils

type symbol = { sym_pos : coord; sym : char }
type part = { part_pos : coord list; num : int }

let symbols_in_row (rowi : int) (row : string) : symbol list =
  let symbol_idx i c =
    match c with
    | '0' .. '9' -> Option.none
    | '.' -> Option.none
    | _ -> Option.some { sym_pos = (rowi, i); sym = c }
  in
  String.explode row |> List.mapi symbol_idx |> List.filter_none

let symbols_in_schematic (schematic : string list) : symbol list =
  List.mapi symbols_in_row schematic |> List.flatten

let parts_in_row (rowi : int) (row : string) : part list =
  let parse_digit d = Char.code d - Char.code '0' in
  let add_to_part part coli digit =
    {
      part_pos = (rowi, coli) :: part.part_pos;
      num = (10 * part.num) + parse_digit digit;
    }
  in
  let accumulate_parts acc (coli, c) =
    match (acc, c) with
    | None, '0' .. '9' ->
        ( Option.some { part_pos = [ (rowi, coli) ]; num = parse_digit c },
          Option.none )
    | None, _ -> (Option.none, Option.none)
    | Some p, '0' .. '9' -> (Option.some (add_to_part p coli c), Option.none)
    | Some p, _ -> (Option.none, Option.some p)
  in
  let digits = String.explode row |> List.mapi Pair.pack in
  let acc, parts = digits |> List.fold_left_map accumulate_parts Option.none in
  let parts = List.filter_none parts in
  match acc with Some p -> p :: parts | None -> parts

let parts_in_schematic (schematic : string list) : part list =
  List.mapi parts_in_row schematic |> List.flatten

let has_adjacent_symbol (symbols : symbol list) (part : part) : bool =
  let symbol_coords = List.map (fun s -> s.sym_pos) symbols in
  let is_symbol c = List.mem c symbol_coords in
  List.map Coord.surrounding part.part_pos
  |> List.flatten |> List.exists is_symbol

let adjacent_parts (parts : part list) (symbol : symbol) : part list =
  let cs = Coord.adjacencies symbol.sym_pos in
  let is_adjacent c = List.mem c cs in
  let is_adjacent_part p = List.exists is_adjacent p.part_pos in
  List.filter is_adjacent_part parts

let gear_ratio (parts : part list) (symbol : symbol) : int option =
  match (symbol.sym, adjacent_parts parts symbol) with
  | '*', p1 :: p2 :: _ -> Option.some (p1.num * p2.num)
  | _ -> Option.none

let puzzle_input () = Io.read_lines "2023/data/03.txt"

let part_one (input : string list) =
  let symbols = symbols_in_schematic input in
  let parts = parts_in_schematic input in
  List.filter (has_adjacent_symbol symbols) parts
  |> List.map (fun p -> p.num)
  |> List.fold_left ( + ) 0

let part_two (input : string list) =
  let symbols = symbols_in_schematic input in
  let parts = parts_in_schematic input in
  List.filter_map (gear_ratio parts) symbols |> List.fold_left ( + ) 0

let _ = Runner.main (puzzle_input ()) part_one part_two
