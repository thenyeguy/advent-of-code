include Stdlib.String

(* Converts a string into a list of its characters. *)
let explode (s : string) : char list = to_seq s |> List.of_seq

(* Folds over the string, character-by-character. *)
let fold_left (f : 'acc -> char -> 'acc) (acc : 'acc) (s : string) : 'acc =
  explode s |> List.fold_left f acc
