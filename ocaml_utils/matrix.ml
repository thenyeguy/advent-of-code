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

let to_seq (m : 'a t) : 'a Seq.t Seq.t = Array.to_seq m |> Seq.map Array.to_seq
let of_seq (s : 'a Seq.t Seq.t) : 'a t = Seq.map Array.of_seq s |> Array.of_seq

(* Converts a matrix of characters into a string *)
let to_string (m : char t) : string =
  let open Fn.Infix in
  Array.map (Array.to_list >> String.implode) m
  |> Array.to_list |> String.concat "\n"

(* Basic accessors: *)
let rows (m : 'a t) : int = Array.length m
let cols (m : 'a t) : int = Array.length m.(0)
let size (m : 'a t) : Coord.t = (rows m, cols m)
let get (m : 'a t) ((row, col) : Coord.t) : 'a = m.(row).(col)
let set (m : 'a t) ((row, col) : Coord.t) (v : 'a) : unit = m.(row).(col) <- v

let in_bounds (m : 'a t) ((row, col) : Coord.t) =
  0 <= row && row < rows m && 0 <= col && col < cols m

let get_opt (m : 'a t) (c : Coord.t) : 'a option =
  if in_bounds m c then Some (get m c) else None

(* Gets all orthogonally adjacent, in-bounds values to [c]. *)
let adjacencies (m : 'a t) (c : Coord.t) : 'a list =
  List.filter_map (get_opt m) (Coord.adjacencies c)

(* Gets all surrounding, in-bounds values to [c]. *)
let surrounding (m : 'a t) (c : Coord.t) : 'a list =
  List.filter_map (get_opt m) (Coord.surrounding c)

let transpose (m : 'a t) : 'a t = to_seq m |> Seq.transpose |> of_seq

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

let fold (f : 'acc -> 'a -> 'acc) (init : 'acc) (m : 'a t) : 'acc =
  Array.fold_left (Array.fold_left f) init m

(* Folds each row/col, and collects the results into a new list *)
let fold_rows (f : 'acc -> 'a -> 'acc) (init : 'acc) (m : 'a t) : 'a list =
  Array.map (Array.fold_left f init) m |> Array.to_list

let fold_cols (f : 'acc -> 'a -> 'acc) (init : 'acc) (m : 'a t) : 'a list =
  m |> transpose |> fold_rows f init

let coords (m : 'a t) : Coord.t list =
  let coord row col _ = (row, col) in
  mapi coord m |> Array.map Array.to_list |> Array.to_list |> List.flatten

(* Scanning *)
let find (f : 'a -> bool) (m : 'a t) : (int * int) option =
  let cols = Array.map (Array.find_index f) m in
  let row = Array.find_index Option.is_some cols in
  let col = Array.fold_left Option.or_ None cols in
  match (row, col) with Some r, Some c -> Some (r, c) | _ -> None

let find_all (f : 'a -> bool) (m : 'a t) : Coord.t list =
  let matching_coords row col value =
    if f value then Some (row, col) else None
  in
  let acc coords coord =
    match coord with Some c -> c :: coords | None -> coords
  in
  mapi matching_coords m |> fold acc []

let findi_all (f : Coord.t -> 'a -> bool) (m : 'a t) : Coord.t list =
  let matching_coords row col value =
    if f (row, col) value then Some (row, col) else None
  in
  let acc coords coord =
    match coord with Some c -> c :: coords | None -> coords
  in
  mapi matching_coords m |> fold acc []

let findi_all_map (f : Coord.t -> 'a -> 'b option) (m : 'a t) :
    (Coord.t * 'b) list =
  let apply row col value =
    match f (row, col) value with
    | Some r -> Some ((row, col), r)
    | None -> None
  in
  let acc coords coord =
    match coord with Some c -> c :: coords | None -> coords
  in
  mapi apply m |> fold acc []

let count (f : 'a -> bool) (m : 'a t) : int =
  let acc sum a = if f a then sum + 1 else sum in
  fold acc 0 m

let counti (f : Coord.t -> 'a -> bool) (m : 'a t) : int =
  let f' r c v = if f (r, c) v then 1 else 0 in
  mapi f' m |> fold ( + ) 0
