open List.Infix

type 'a t = 'a array array

let make = Array.make_matrix
let init = Array.init_matrix
let copy m = Array.map Array.copy m

(* Converts lines of strings into a matrix of characters. *)
let of_strings (lines : string list) : char t =
  lines ||> String.explode ||> Array.of_list |> Array.of_list

(* Converts a string into a matrix of characters. *)
let of_string (s : string) : char t =
  s |> String.split_on_char '\n' |> of_strings

let rows (m : 'a t) : int = Array.length m
let cols (m : 'a t) : int = Array.length m.(0)
let size (m : 'a t) : Coord.t = (rows m, cols m)
let get (m : 'a t) ((row, col) : Coord.t) : 'a = m.(row).(col)
let set (m : 'a t) ((row, col) : Coord.t) (v : 'a) : unit = m.(row).(col) <- v

let in_bounds (m : 'a t) ((row, col) : Coord.t) =
  0 <= row && row < rows m && 0 <= col && col < cols m

let get_opt (m : 'a t) (c : Coord.t) : 'a option =
  if in_bounds m c then Option.some (get m c) else Option.none

let transpose (m : 'a t) : 'a t =
  m |> Array.to_seq |> Seq.map Array.to_seq |> Seq.transpose
  |> Seq.map Array.of_seq |> Array.of_seq

(* Rotates the matrix clockwise. *)
let rotate (m : 'a t) : 'a t =
  let rs, cs = size m in
  let new_value r c = get m (rs - c - 1, r) in
  init cs rs new_value

(* Iterators: *)
let map (f : 'a -> 'b) (m : 'a t) : 'b t = Array.map (Array.map f) m

let mapi (f : int -> int -> 'a -> 'b) (m : 'a t) : 'b t =
  Array.mapi (fun row -> Array.mapi (f row)) m

let iteri (f : int -> int -> 'a -> unit) (m : 'a t) : unit =
  let _ = mapi f m in
  ()

let fold (f : 'acc -> 'a -> 'acc) (acc : 'acc) (m : 'a t) : 'acc =
  let rows = Array.map (Array.fold_left f acc) m in
  Array.fold_left f acc rows

(* Folds each row/col, and collects the results into a new list *)
let fold_rows (f : 'acc -> 'a -> 'acc) (acc : 'acc) (m : 'a t) : 'a list =
  Array.map (Array.fold_left f acc) m |> Array.to_list

let fold_cols (f : 'acc -> 'a -> 'acc) (acc : 'acc) (m : 'a t) : 'a list =
  m |> transpose |> fold_rows f acc

(* Scanning *)
let find (f : 'a -> bool) (m : 'a t) : (int * int) option =
  let cols = Array.map (Array.find_index f) m in
  let row = Array.find_index Option.is_some cols in
  let col = Array.fold_left Option.or_ Option.none cols in
  match (row, col) with
  | Some r, Some c -> Option.some (r, c)
  | _ -> Option.none
