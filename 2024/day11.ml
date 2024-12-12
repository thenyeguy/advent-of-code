open Utils
open Utils.Fn.Infix
module IntMap = Map.Make (Int)

(*
 * Parse input
 *)
let puzzle_input = Io.read_file "2024/data/11.txt" |> List.of_string

(*
 * Part 1
 *)
let rec to_digits (i : int) : int Seq.t =
  if i < 10 then Seq.return i else Seq.cons (i mod 10) (to_digits (i / 10))

let rec from_digits (digits : int Seq.t) : int =
  match Seq.uncons digits with
  | Some (d, ds) -> d + (10 * from_digits ds)
  | _ -> 0

let blink (stone : int) : int list =
  if stone = 0 then [ 1 ]
  else
    let digits = to_digits stone in
    let n = Seq.length digits in
    if n mod 2 = 0 then
      let l, r = (Seq.take (n / 2) digits, Seq.drop (n / 2) digits) in
      [ from_digits l; from_digits r ]
    else [ stone * 2024 ]

let blink_stones ?(times : int = 1) (counts : int IntMap.t) : int IntMap.t =
  let rec blink_stones_inner i counts =
    if i = 0 then counts
    else
      let acc stone count counts =
        blink stone |> List.fold_left (IntMap.increment ~count) counts
      in
      let counts' = IntMap.fold acc counts IntMap.empty in
      blink_stones_inner (i - 1) counts'
  in
  blink_stones_inner times counts

let num_stones (counts : int IntMap.t) : int =
  let acc _ count total = total + count in
  IntMap.fold acc counts 0

let part_one = IntMap.counts >> blink_stones ~times:25 >> num_stones

(*
 * Part 2
 *)
let part_two = IntMap.counts >> blink_stones ~times:75 >> num_stones

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
