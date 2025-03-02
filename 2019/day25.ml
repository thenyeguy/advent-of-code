open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/25.txt"

(*
 * Interactive mode:
 *)
let run_interpreter (c : Intcode.Computer.t) =
  let open Intcode in
  let tick c = Computer.run c |> Ascii.read_all in
  let rec run c =
    try
      tick c |> print_string;
      Ascii.write_line c (read_line ());
      run c
    with End_of_file -> ()
  in
  run c

(*
 * Part 1
 *)
type room = { name : string; doors : string list; items : string list }

let opposite (dir : string) : string =
  match dir with
  | "north" -> "south"
  | "south" -> "north"
  | "west" -> "east"
  | "east" -> "west"

let is_opposite (src : string) (dst : string) : bool = opposite src = dst

let parse (output : string) : room =
  let strip_heading s = String.sub s 3 (String.length s - 6) in
  let strip_bullet s = String.sub s 2 (String.length s - 2) in
  let is_dir s =
    match s with "north" | "south" | "east" | "west" -> true | _ -> false
  in
  let lines = String.split_on_char '\n' output in
  let name =
    List.find (String.starts_with ~prefix:"==") lines |> strip_heading
  in
  let l = List.filter (String.starts_with ~prefix:"-") lines ||> strip_bullet in
  { name; doors = List.filter is_dir l; items = List.filter (not << is_dir) l }

let tick (c : Intcode.Computer.t) : string =
  Intcode.Computer.run c |> Intcode.Ascii.read_all

let move (c : Intcode.Computer.t) (dir : string) : room =
  Intcode.Ascii.write_line c dir;
  tick c |> parse

let inv (c : Intcode.Computer.t) =
  Intcode.Ascii.write_line c "inv";
  ignore (tick c)

let take (c : Intcode.Computer.t) (item : string) =
  Intcode.Ascii.write c "take ";
  Intcode.Ascii.write_line c item;
  ignore (tick c)

let drop (c : Intcode.Computer.t) (item : string) =
  Intcode.Ascii.write c "drop ";
  Intcode.Ascii.write_line c item;
  ignore (tick c)

let init (prog : Intcode.Computer.program_t) : Intcode.Computer.t * room =
  let c = Intcode.Computer.init prog in
  (c, tick c |> parse)

let find_items (c : Intcode.Computer.t) (room : room) : string list =
  let is_valid item =
    match item with
    | "escape pod" | "infinite loop" | "giant electromagnet" | "molten lava"
    | "photons" ->
        false
    | _ -> true
  in
  let rec explore items dir =
    let room = move c dir in
    let items' =
      if room.name = "Security Checkpoint" then items
      else
        let dirs = List.filter (not << is_opposite dir) room.doors in
        let items' = List.filter is_valid room.items in
        List.iter (take c) items';
        List.fold_left explore (items @ items') dirs
    in
    ignore (move c (opposite dir));
    items'
  in
  List.fold_left explore room.items room.doors

let find_checkpoint (c : Intcode.Computer.t) (room : room) =
  let exception Found in
  let rec explore dir =
    let room = move c dir in
    ignore
      (if room.name = "Security Checkpoint" then raise Found
       else
         let dirs = List.filter (not << is_opposite dir) room.doors in
         List.iter explore dirs);
    ignore (move c (opposite dir))
  in
  try List.iter explore room.doors with Found -> ()

let find_passcode (c : Intcode.Computer.t) (items : string list) : int =
  let exception Found of int in
  let test () =
    let output =
      Intcode.Ascii.write_line c "south";
      tick c
    in
    let words = String.split_on_char ' ' output in
    match List.find_map int_of_string_opt words with
    | Some passcode -> raise (Found passcode)
    | None -> ()
  in
  let rec find' items =
    match items with
    | item :: items' ->
        drop c item;
        test ();
        find' items';
        take c item;
        find' items'
    | _ -> ()
  in
  try
    find' items;
    raise Not_found
  with Found passcode -> passcode

let part_one input =
  let c, starting_room = init input in
  let items = find_items c starting_room in
  find_checkpoint c starting_room;
  find_passcode c items

(*
 * Main
 *)
let main () =
  if Array.exists (( = ) "--interactive") Sys.argv then
    run_interpreter (Intcode.Computer.init puzzle_input)
  else Runner.main puzzle_input part_one Runner.unimplemented

let _ = if not !Sys.interactive then main ()
