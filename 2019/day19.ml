open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/19.txt"

(*
 * Part 1
 *)
let in_tractor_beam (prog : Intcode.Computer.program_t) ((row, col) : coord) :
    bool =
  let [ out ] = Intcode.Computer.run_program prog [ col; row ] in
  out = 1

let part_one input =
  let coords =
    List.concat_map (fun x -> List.range 50 ||> Pair.pack x) (List.range 50)
  in
  List.count (in_tractor_beam input) coords

(*
 * Part 2
 *)
let rec find_first ?(min : int = 0) (prog : Intcode.Computer.program_t)
    (row : int) : int =
  (* Printf.printf "  find_first %d %d\n" min row; *)
  if in_tractor_beam prog (row, min) then min
  else find_first ~min:(min + 1) prog row

let first_coords (prog : Intcode.Computer.program_t) (from : int) (to_ : int) :
    coord list =
  let first min row =
    (* Printf.printf "first_coords %d %d\n" min row; *)
    let col = find_first ~min prog row in
    (col, (row, col))
  in
  List.range ~from to_ |> List.fold_left_map first 0 |> snd

let fits ?(size : int = 100) (prog : Intcode.Computer.program_t)
    ((row, col) : coord) : coord option =
  let in_beam r c = in_tractor_beam prog (r, c) in
  let upper_row = row - size + 1 in
  if
    in_beam row (col + size - 1)
    && in_beam upper_row col
    && in_beam upper_row (col + size - 1)
  then Some (upper_row, col)
  else None

let first_fit (prog : Intcode.Computer.program_t) : int * int =
  first_coords prog 1000 10000 |> List.find_map (fits prog) |> Option.get

let part_two input =
  let y, x = first_fit input in
  (10000 * x) + y

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
