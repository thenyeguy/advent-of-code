open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/17.txt"

(*
 * Part 1
 *)
let load_scaffold (prog : Intcode.Computer.program_t) : char matrix =
  Intcode.Computer.run_program prog []
  ||> char_of_int |> String.implode |> String.trim |> Matrix.of_string

let find_intersections (scaffold : char matrix) =
  let is_intersection (pos : coord) (c : char) : bool =
    let open Coord in
    c = '#'
    && dirs ||> step pos ||> Matrix.get_opt scaffold
       |> List.all (( = ) (Some '#'))
  in
  Matrix.findi_all is_intersection scaffold

let compute_alignment (scaffold : char matrix) : int =
  let acc total (row, col) = total + (row * col) in
  find_intersections scaffold |> List.fold_left acc 0

let part_one = load_scaffold >> compute_alignment

(*
 * Part 2
 *)
type movement = Forward of int | TurnLeft | TurnRight | Routine of string

(* Find a path greedily by minimizing turns. *)
let find_path (scaffold : char matrix) : movement list =
  let open Coord in
  let next_turn pos dir =
    let left = turn_left dir |> step pos |> Matrix.get_opt scaffold in
    let right = turn_right dir |> step pos |> Matrix.get_opt scaffold in
    match (left, right) with
    | Some '#', _ -> Some (TurnLeft, turn_left dir)
    | _, Some '#' -> Some (TurnRight, turn_right dir)
    | _ -> None
  in
  let count_steps pos dir =
    let rec count_steps_inner pos steps =
      let pos' = step pos dir in
      match Matrix.get_opt scaffold pos' with
      | Some '#' -> count_steps_inner pos' (steps + 1)
      | _ -> (Forward steps, pos)
    in
    count_steps_inner pos 0
  in
  let rec traverse pos dir =
    match next_turn pos dir with
    | None -> []
    | Some (turn, dir) ->
        let move, pos = count_steps pos dir in
        turn :: move :: traverse pos dir
  in
  let (Some start) = Matrix.find (( = ) '^') scaffold in
  traverse start Up

let rec starts_with (needle : 'a list) (haystack : 'a list) : bool =
  match (needle, haystack) with
  | [], _ -> true
  | _, [] -> false
  | n :: ns, h :: hs -> if h = n then starts_with ns hs else false

let find_substring (needle : 'a list) (haystack : 'a list) : int list =
  let rec traverse i hs =
    match hs with
    | [] -> []
    | _ :: tl as hs ->
        if starts_with needle hs then i :: traverse (i + 1) tl
        else traverse (i + 1) tl
  in
  traverse 0 haystack

let largest_recurring_prefix (haystack : 'a list) : 'a list =
  let rec try_next n h =
    match h with
    | [] -> None
    | h :: hs ->
        let n' = n @ [ h ] in
        let substrings = find_substring n' haystack in
        if List.length substrings > 1 then
          match try_next n' hs with Some r -> Some r | None -> Some n'
        else None
  in
  try_next [] haystack |> Option.get

let replace (haystack : 'a list) (needle : 'a list) (value : 'a) : 'a list =
  let rec replace_inner haystack =
    match haystack with
    | [] -> []
    | hd :: tl ->
        if starts_with needle haystack then
          let tl = List.drop (List.length needle) haystack in
          value :: replace_inner tl
        else hd :: replace_inner tl
  in
  replace_inner haystack

let force_even (l : 'a list) : 'a list =
  let len = List.length l in
  if len mod 2 = 0 then l
  else List.to_seq l |> Seq.take (len - 1) |> List.of_seq

let string_of_moves (moves : movement list) : string =
  let to_string move =
    match move with
    | Forward n -> string_of_int n
    | TurnLeft -> "L"
    | TurnRight -> "R"
    | Routine name -> name
  in
  List.map to_string moves |> String.concat ","

(* Greedily build a program by looking for the largest function from the head. *)
let build_routine (moves : movement list) : string =
  let extract_functions (moves : movement list) : movement list list =
    let extract_function name moves =
      (* Exclude Routine moves to prevent recursion.
       * Always take even numbers of moves to ensure (turn, move) pairs. *)
      let is_routine m = match m with Routine _ -> true | _ -> false in
      let f =
        largest_recurring_prefix (List.drop_while is_routine moves)
        |> force_even
        |> List.take_while (not << is_routine)
      in
      (f, replace moves f (Routine name))
    in
    let fa, moves = extract_function "A" moves in
    let fb, moves = extract_function "B" moves in
    let fc, moves = extract_function "C" moves in
    [ moves; fa; fb; fc ]
  in
  extract_functions moves ||> string_of_moves |> String.concat "\n"

let run_routine (routine : string) : int =
  let prog = 2 :: List.tl puzzle_input in
  let input = routine ^ "\nn\n" |> String.explode ||> int_of_char in
  Intcode.Computer.run_program prog input |> List.last

let part_two input =
  load_scaffold input |> find_path |> build_routine |> run_routine

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
