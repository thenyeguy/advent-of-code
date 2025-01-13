open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_file "2019/data/08.txt" |> String.explode

(*
 * Part 1
 *)
type 'a tensor = 'a array array array

let to_tensor ?(cols : int = 25) ?(rows : int = 6) (l : 'a list) : 'a tensor =
  let open Seq in
  let layer_size = cols * rows in
  let rec reshape (n : int) (s : 'a Seq.t) : 'a Seq.t Seq.t =
    if is_empty s then empty else cons (take n s) (reshape n (drop n s))
  in
  let to_matrix (s : 'a Seq.t) : 'a array array =
    reshape cols s |> map Array.of_seq |> Array.of_seq
  in
  reshape layer_size (List.to_seq l) |> map to_matrix |> Array.of_seq

let count (c : char) (m : char matrix) : int =
  let acc total c' = if c = c' then total + 1 else total in
  Matrix.fold acc 0 m

let find_min_layer (tensor : char tensor) : char matrix =
  let zeros = count '0' in
  let acc min_layer layer =
    if zeros layer < zeros min_layer then layer else min_layer
  in
  Array.fold_left acc tensor.(0) tensor

let part_one input =
  let min_layer = to_tensor input |> find_min_layer in
  count '1' min_layer * count '2' min_layer

(*
 * Part 2
 *)
let flatten_image (tensor : char tensor) : char matrix =
  let get_pixel row col =
    let flatten acc c = if acc = '2' then c else acc in
    Array.map (Fn.flip Matrix.get (row, col)) tensor
    |> Array.fold_left flatten '2'
  in
  let rows, cols = Matrix.size tensor.(0) in
  Matrix.init rows cols get_pixel

let to_string (m : char matrix) : string =
  let to_block c = if c = '1' then '#' else ' ' in
  m |> Matrix.map to_block |> Matrix.to_string

let part_two = to_tensor >> flatten_image >> to_string >> String.cat "\n"

(*
 * Main
 *)
let _ = Runner.string_main puzzle_input (part_one >> string_of_int) part_two
