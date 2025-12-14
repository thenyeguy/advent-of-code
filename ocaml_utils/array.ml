include Stdlib.Array

(* Checks if any element in [l] matches the predicate [f]. *)
let any (f : 'a -> bool) (a : 'a array) : bool =
  let acc result elem = result || f elem in
  fold_left acc false a

(* Checks if all elements in [l] matches the predicate [f]. *)
let all (f : 'a -> bool) (a : 'a array) : bool =
  let acc result elem = result && f elem in
  fold_left acc true a

(* Integer reducers: *)
let sum (a : int array) : int = fold_left ( + ) 0 a
let product (a : int array) : int = fold_left ( * ) 1 a
let max (a : int array) : int = fold_left max min_int a
let min (a : int array) : int = fold_left min max_int a

(* Float reducers: *)
let fsum (a : float array) : float = fold_left ( +. ) 0.0 a
let fproduct (a : float array) : float = fold_left ( *. ) 1.0 a
let fmax (a : float array) : float = fold_left Float.max min_float a
let fmin (a : float array) : float = fold_left Float.min max_float a
