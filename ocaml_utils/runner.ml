open Fn.Infix

exception Unimplemented

(* Dummy function for unimplemented solutions. *)
let unimplemented _ = raise Unimplemented

(* Util function for running AOC solutions. *)
let runner (input : 'a) (part_one : 'a -> unit) (part_two : 'a -> unit) =
  if not !Sys.interactive then (
    let run label f =
      try
        print_string label;
        f input;
        print_newline ()
      with Unimplemented -> ()
    in
    run "Part one: " part_one;
    run "Part two: " part_two)

let main (input : 'a) (part_one : 'a -> int) (part_two : 'a -> int) =
  runner input (part_one >> print_int) (part_two >> print_int)

let string_main (input : 'a) (part_one : 'a -> string) (part_two : 'a -> string)
    =
  runner input (part_one >> print_string) (part_two >> print_string)
