include Stdlib.String

(* Converts a string into a list of its characters. *)
let explode (s : string) : char list = to_seq s |> List.of_seq
