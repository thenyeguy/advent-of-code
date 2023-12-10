type 'a solution = 'a -> int

exception Unimplemented

(* Dummy function for unimplemented solutions. *)
let unimplemented (_input : 'a) : int = raise Unimplemented

(* Util function for running AOC solutions. *)
let main (input : 'a) (part_one : 'a solution) (part_two : 'a solution) =
  if not !Sys.interactive then (
    let run label f =
      try
        let solution = f input in
        print_string label;
        print_int solution;
        print_newline ()
      with Unimplemented -> ()
    in
    run "Part one: " part_one;
    run "Part two: " part_two)
