open Computer
open Utils

(** Sends the given string as input to the computer. *)
let write (c : Computer.t) (s : string) =
  String.explode s ||> int_of_char ||> push c |> ignore

(** Sends the given string followed by a newline as input to the computer. *)
let write_line (c : Computer.t) (s : string) =
  write c s;
  push c 10 (* ascii newline *)

(** Reads all available output into a multiline string. *)
let read_all (c : Computer.t) : string =
  let rec read_inner () : char list =
    if has_output c then
      let char = char_of_int (pop c) in
      char :: read_inner ()
    else []
  in
  read_inner () |> String.implode

(** Reads a single line of available output. *)
let read_line (c : Computer.t) : string =
  let rec read_inner () : char list =
    let char = char_of_int (pop c) in
    if char = '\n' then [] else char :: read_inner ()
  in
  read_inner () |> String.implode
