include Stdlib.List

(* Convenient reducers: *)
let any (l : bool list) : bool = fold_left ( || ) false l
let all (l : bool list) : bool = fold_left ( && ) true l
let sum (l : int list) : int = fold_left ( + ) 0 l
let product (l : int list) : int = fold_left ( * ) 1 l

(* Get alternating elements: *)
let rec evens (ls : 'a list) : 'a list =
  match ls with a :: _ :: tail -> a :: evens tail | _ -> []

let rec odds (ls : 'a list) : 'a list =
  match ls with _ :: b :: tail -> b :: odds tail | _ -> []

(* Building new lists: *)
let repeated (value : 'a) (len : int) : 'a list = init len (fun _ -> value)
