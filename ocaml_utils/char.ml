include Stdlib.Char

(** Converts an ASCII digit to its integer value. *)
let to_digit c = code c - code '0'

(* Creates a list containing characters [from,to) *)
let range (from : char) (to_ : char) : char list =
  List.range ~from:(int_of_char from) (int_of_char to_) |> List.map char_of_int

(* Creates a list containing integers [from,to] *)
let irange (from : char) (to_ : char) : char list =
  List.irange ~from:(int_of_char from) (int_of_char to_) |> List.map char_of_int
