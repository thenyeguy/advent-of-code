open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_file "2024/data/04.txt" |> Matrix.of_string

(*
 * Part 1
 *)
let rec get_slice ~(l : int) (m : char matrix) (c : coord) (v : Coord.vec) :
    char list option =
  if l = 0 then Some []
  else
    match Matrix.get_opt m c with
    | Some ch -> (
        match get_slice ~l:(l - 1) m (Coord.add c v) v with
        | Some chs -> Some (ch :: chs)
        | _ -> None)
    | None -> None

let get_slices ~(l : int) (m : char matrix) (c : coord) : string list =
  List.filter_map (get_slice ~l m c) Coord.all_direction_vecs ||> String.implode

let count_xmas (m : char matrix) : int =
  let count_slices row col c =
    if c = 'X' then get_slices ~l:4 m (row, col) |> List.count (( = ) "XMAS")
    else 0
  in
  Matrix.mapi count_slices m |> Matrix.fold ( + ) 0

let part_one = count_xmas

(*
 * Part 2
 *)
let get_range (m : char matrix) ((row, col) : coord) : char Matrix.t =
  let mget r c = Matrix.get m (row + r, col + c) in
  Matrix.init 3 3 mget

let is_x_mas (m : char matrix) : bool =
  let is_x_mas' m' =
    let g = Matrix.get m' in
    g (0, 0) = 'M'
    && g (0, 2) = 'M'
    && g (1, 1) = 'A'
    && g (2, 0) = 'S'
    && g (2, 2) = 'S'
  in
  let m' = Matrix.rotate m in
  let m'' = Matrix.rotate m' in
  let m''' = Matrix.rotate m'' in
  is_x_mas' m || is_x_mas' m' || is_x_mas' m'' || is_x_mas' m'''

let count_x_mas (m : char matrix) : int =
  let rs, cs = Matrix.size m in
  let count_x_mas r c _ =
    if r < rs - 2 && c < cs - 2 then
      if is_x_mas (get_range m (r, c)) then 1 else 0
    else 0
  in
  Matrix.mapi count_x_mas m |> Matrix.fold ( + ) 0

let part_two = count_x_mas

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
