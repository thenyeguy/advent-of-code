include Stdlib.String

(* Converts a string to a list of its lines. *)
let lines : string -> string list = split_on_char '\n'

(* Converts a string to a list of its words. *)
let words : string -> string list = split_on_char ' '

(* Converts a string into a list of its characters. *)
let explode (s : string) : char list = to_seq s |> List.of_seq

(* Converts a list characters into a string. *)
let implode (cs : char list) : string = List.to_seq cs |> of_seq

(* Takes the first [n] characters of [s] *)
let take (n : int) (s : string) : string = explode s |> List.take n |> implode

(* Drops the first [n] characters of [s] *)
let drop (n : int) (s : string) : string = explode s |> List.drop n |> implode

(* Folds over the string, character-by-character. *)
let fold_left (f : 'acc -> char -> 'acc) (acc : 'acc) (s : string) : 'acc =
  explode s |> List.fold_left f acc
