open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/10.txt"

(*
 * Part 1
 *)
type machine = {
  lights : bool array;
  buttons : int list list;
  joltages : int array;
}

let parse (line : string) : machine =
  let parse_lights s =
    let f c = match c with '.' -> Some false | '#' -> Some true | _ -> None in
    (String.explode >> List.filter_map f) s
  in
  let parse_nums s =
    let s' = String.sub s 1 (String.length s - 2) in
    List.of_string ~sep:',' s'
  in
  let words = String.split_on_char ' ' line in
  let lights = List.hd words |> parse_lights |> Array.of_list in
  let buttons =
    (List.drop 1 >> List.rev >> List.drop 1 >> List.rev) words
    |> List.map parse_nums
  in
  let joltages = List.last words |> parse_nums |> Array.of_list in
  { lights; buttons; joltages }

let fewest_presses (m : machine) : int =
  let rec search lights buttons =
    if lights = m.lights then 0
    else
      match buttons with
      | [] -> max_int / 2
      | b :: bs ->
          let lights' = Array.copy lights in
          let _ = List.iter (fun i -> lights'.(i) <- not lights'.(i)) b in
          min (search lights bs) (1 + search lights' bs)
  in
  let lights = Array.init (Array.length m.lights) (Fn.const false) in
  search lights m.buttons

let part_one = List.map (parse >> fewest_presses) >> List.sum

(*
 * Part 2
 *)
let button_matrix (machine : machine) : float matrix =
  let rows = List.length machine.buttons in
  let cols = Array.length machine.joltages in
  let m = Matrix.make rows cols 0.0 in
  let add_button row buttons =
    List.iter (fun col -> Matrix.set m (row, col) 1.0) buttons
  in
  List.iteri add_button machine.buttons;
  m

let find_pivots (m : float matrix) : int list * int list =
  let pivots =
    Array.map (Array.find_index (( = ) 1.0)) m
    |> Array.to_list |> List.filter_none
  in
  let is_free i = List.exists (( = ) i) pivots |> not in
  (pivots, List.range (Matrix.cols m - 1) |> List.filter is_free)

let maybe_solve (m : float matrix) (a : float array) : int array option =
  if Array.any (Fn.lt 0.0) a then None
  else
    let cols = Matrix.cols m in
    let m' = Lin_alg.augment m a in
    Lin_alg.to_row_echelon_form m';
    let is_valid f = Float.abs (f -. Float.round f) <= 0.0001 && f >= -0.0001 in
    let x = Matrix.col m' cols in
    if Array.all is_valid x then
      Some (Array.map (Float.round >> int_of_float) x)
    else None

let find_presses_search ?(n : int = 10) (m : float matrix)
    (solution : float array) (buttons : float array list) : int =
  let rec search s bs =
    match bs with
    | [] -> begin
        match maybe_solve m s with Some s -> Array.sum s | None -> max_int / 2
      end
    | b :: bs' ->
        let acc (s, min_presses) i =
          if Array.any (Fn.lt 0.0) s then (s, min_presses)
          else
            let sub s b = s -. b in
            let presses = i + search s bs' in
            (Array.map2 sub s b, min min_presses presses)
        in
        List.range n |> List.fold_left acc (s, max_int) |> snd
  in
  search solution buttons

let solve (mach : machine) =
  let buttons = button_matrix mach in
  let joltages = Array.map float_of_int mach.joltages in
  (* Perform gaussian elimination to reduce: *)
  let m = Lin_alg.augment (Matrix.transpose buttons) joltages in
  Lin_alg.to_row_echelon_form m;
  (* Find fixed and free variables *)
  let pivots, frees = find_pivots m in
  let fixed_matrix =
    pivots ||> Array.get buttons |> Array.of_list |> Matrix.transpose
  in
  let free_buttons = List.map (Array.get buttons) frees in
  (* Search over the free variables: *)
  let range = Array.max mach.joltages + 1 in
  find_presses_search ~n:range fixed_matrix joltages free_buttons

let part_two = List.map (parse >> solve) >> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
