include Stdlib.Char

(** Converts an ASCII digit to its integer value. *)
let to_digit c = code c - code '0'
