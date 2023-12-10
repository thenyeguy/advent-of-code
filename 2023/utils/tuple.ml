(* Util functions for unpacking tuple elements. *)
let left (l, _) = l
let right (_, r) = r

(* Util functions for packing tuples. *)
let pack l r = (l, r)
