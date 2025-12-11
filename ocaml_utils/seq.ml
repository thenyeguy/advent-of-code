include Stdlib.Seq

(* Counts all elements matching the predicate. *)
let count (f : 'a -> bool) (l : 'a t) : int =
  fold_left (fun c v -> c + if f v then 1 else 0) 0 l

(* Creates a seq containing integers [from,to) *)
let range ?(from : int = 0) (to_ : int) : int t =
  if from < to_ then init (to_ - from) (fun i -> from + i)
  else init (from - to_) (fun i -> from - i)

(* Creates a seq containing integers [from,to] *)
let irange ?(from : int = 0) (to_ : int) : int t =
  if from < to_ then range ~from (to_ + 1) else range ~from (to_ - 1)
