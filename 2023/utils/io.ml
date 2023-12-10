(* Reads a file into a list line-by-line. *)
let read_lines (fname : string) : string list =
  let rec _read_lines (ch : in_channel) : string list =
    try
      let line = input_line ch in
      line :: _read_lines ch
    with End_of_file -> []
  in
  let ic = open_in fname in
  let lines = _read_lines ic in
  close_in ic;
  lines
