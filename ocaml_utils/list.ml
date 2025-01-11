include Stdlib.List

(* Creates a list with `len` copies of `value`. *)
let repeated (value : 'a) (len : int) : 'a list = init len (fun _ -> value)

(* Creates a list containing integers [from,to) *)
let range ?(from : int = 0) (to_ : int) : int list =
  if from < to_ then init (to_ - from) (fun i -> from + i)
  else init (from - to_) (fun i -> from - i)

(* Creates a list containing integers [from,to] *)
let irange ?(from : int = 0) (to_ : int) : int list =
  if from < to_ then init (to_ - from + 1) (fun i -> from + i)
  else init (from - to_ + 1) (fun i -> from - i)

(* Adds indices to a list *)
let enumerate (l : 'a list) : (int * 'a) list = mapi Pair.pack l

(* Getters: *)
let rec last (l : 'a list) : 'a =
  match l with [] -> raise (Failure "last") | [ x ] -> x | _ :: xs -> last xs

(* Convenient reducers: *)
let any (l : bool list) : bool = fold_left ( || ) false l
let all (l : bool list) : bool = fold_left ( && ) true l
let sum (l : int list) : int = fold_left ( + ) 0 l
let product (l : int list) : int = fold_left ( * ) 1 l
let max (l : int list) : int = fold_left max min_int l
let min (l : int list) : int = fold_left min max_int l

(* Returns the (index, value) of the minimum element. *)
let mini (l : int list) : int * int =
  let acc (mini, minx) (i, x) = if x < minx then (i, x) else (mini, minx) in
  fold_left acc (-1, max_int) (enumerate l)

(* Returns the (index, value) of the maximum element. *)
let maxi (l : int list) : int * int =
  let acc (maxi, maxx) (i, x) = if x > maxx then (i, x) else (maxi, maxx) in
  fold_left acc (-1, min_int) (enumerate l)

(* Counts all elements matching the predicate. *)
let count (f : 'a -> bool) (l : 'a list) : int =
  fold_left (fun c v -> c + if f v then 1 else 0) 0 l

(* Filter out None elements. *)
let filter_none (l : 'a option list) : 'a list = filter_map Fn.id l

(* Get alternating elements: *)
let rec evens (ls : 'a list) : 'a list =
  match ls with a :: _ :: tail -> a :: evens tail | _ -> []

let rec odds (ls : 'a list) : 'a list =
  match ls with _ :: b :: tail -> b :: odds tail | _ -> []

(* Maps over pairs of elements from the list. *)
let pair_map (f : 'a -> 'a -> 'b) (l : 'a list) =
  let head = to_seq l in
  let tail = to_seq (tl l) in
  Seq.zip tail head |> Seq.map (fun (l, r) -> f l r) |> of_seq

(* List equivalent to Seq.group. *)
let group (g : 'a -> 'a -> bool) (l : 'a list) : 'a list list =
  l |> to_seq |> Seq.group g |> of_seq |> map of_seq

(* Triple equivalents to pair operations: *)
let split3 (l : ('a * 'b * 'c) list) : 'a list * 'b list * 'c list =
  let xs (x, _, _) = x in
  let ys (_, y, _) = y in
  let zs (_, _, z) = z in
  (map xs l, map ys l, map zs l)

let combine3 (xs : 'a list) (ys : 'b list) (zs : 'c list) : ('a * 'b * 'c) list
    =
  let flatten (a, (b, c)) = (a, b, c) in
  combine ys zs |> combine xs |> map flatten

(* Returns a list of all combinations of the elements in `list` *)
let rec combinations (list : 'a list) : ('a * 'a) list =
  match list with
  | [] -> []
  | l :: ls -> append (map (Pair.pack l) ls) (combinations ls)

(* Parses a string into a list. * Defaults to an `int list`. *)
let of_string ?(sep : char = ' ') ?(f : string -> 'a = int_of_string)
    (s : string) : 'a list =
  let open Fn.Infix in
  Stdlib.String.split_on_char sep s |> Stdlib.List.map (Stdlib.String.trim >> f)

module Infix = struct
  (* List.map operator: *)
  let ( ||> ) (l : 'a list) (f : 'a -> 'b) : 'b list = map f l

  (* Append operator: *)
  let ( @ ) = append
end
