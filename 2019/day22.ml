open Utils

(*
 * Parse input
 *)
type technique = Reverse | Cut of int | Deal of int

let parse (line : string) : technique =
  if line = "deal into new stack" then Reverse
  else
    let n = String.split_on_char ' ' line |> List.last |> int_of_string in
    if String.starts_with ~prefix:"cut" line then Cut n else Deal n

let puzzle_input = Io.read_lines "2019/data/22.txt" ||> parse

(*
 * Part 1
 *)
let rec cut (deck : int list) (cards : int) : int list =
  let open Seq in
  if cards < 0 then cut deck (List.length deck + cards)
  else
    let s = List.to_seq deck in
    append (drop cards s) (take cards s) |> List.of_seq

let deal (deck : int list) (inc : int) : int list =
  let len = List.length deck in
  let a = Array.make len 0 in
  let rec deal_inner ds i =
    match ds with
    | [] -> ()
    | d :: ds ->
        a.(i) <- d;
        deal_inner ds ((i + inc) mod len)
  in
  deal_inner deck 0;
  Array.to_list a

let apply_technique (deck : int list) (t : technique) : int list =
  match t with
  | Reverse -> List.rev deck
  | Cut cards -> cut deck cards
  | Deal inc -> deal deck inc

let shuffle (ts : technique list) (deck : int list) : int list =
  List.fold_left apply_technique deck ts

let part_one input =
  let deck = List.range 10007 in
  shuffle input deck |> List.find_index (( = ) 2019) |> Option.get

(*
 * Part 2
 *)
type i128 = Stdint.Int128.t

(* Redefine mod to be positive. *)
let ( mod ) (x : i128) (y : i128) : i128 =
  let open Stdint.Int128 in
  let m = rem x y in
  if m < zero then m + y else m

(* Computes a b^e mod m. *)
let pow ~(m : i128) (b : i128) (e : i128) : i128 =
  let open Stdint.Int128 in
  let two = of_int 2 in
  let rec pow_inner r b e =
    if e = zero then r
    else
      let r = if e mod two = one then r * b mod m else r in
      pow_inner r (b * b mod m) (e / two)
  in
  pow_inner one b e

(* Computes 1 / n mod m
 *   Fermat's little theorem implies:
 *     -1 = m-2  mod m-1
 *     -> a^-1 = a^(m-2)  mod m
 *)
let invmod ~(m : i128) (n : i128) : i128 =
  let open Stdint.Int128 in
  pow ~m n (m - of_int 2)

(* y = mult * x + offset *)
type shift = { mult : i128; offset : i128 }

let id = Stdint.Int128.{ mult = one; offset = zero }

(* Reverse applies a given technique (to unshuffle). *)
let reverse_apply_technique ~(m : i128) ({ mult; offset } : shift)
    (t : technique) : shift =
  let open Stdint.Int128 in
  match t with
  | Reverse ->
      { mult = (zero - mult) mod m; offset = (zero - offset - one) mod m }
  (* forward: { mult; offset = offset - n } *)
  | Cut n -> { mult; offset = (offset + of_int n) mod m }
  (* forward: { mult = n * mult; offset = n * offset } *)
  | Deal n ->
      let n = invmod ~m (of_int n) in
      { mult = n * mult mod m; offset = n * offset mod m }

let reverse_shuffle (size : int) (ts : technique list) : shift =
  let m = Stdint.Int128.of_int size in
  List.fold_left (reverse_apply_technique ~m) id (List.rev ts)

(* Repeatedly apply the shift analytically.
 *   x_1 = mx + o
 *   x_2 = m^2x + mo + o
 *   x_3 = m^3x + m^2o + mo + o
 *   x_n = m^n x + (sum 1 n m^i) o
 *       = m^n x + (m^n - 1) / (m - 1) * o
 *)
let repeat ~(m : int) (times : int) ({ mult; offset } : shift) : shift =
  let open Stdint.Int128 in
  let m, times = (of_int m, of_int times) in
  let mult' = pow ~m mult times in
  let offset = offset * (mult' - one) mod m * invmod ~m (mult - one) mod m in
  { mult = mult'; offset }

let get ~(m : int) ({ mult; offset } : shift) (i : int) : int =
  let open Stdint.Int128 in
  ((mult * of_int i) + offset) mod of_int m |> to_int

let part_two input =
  let m = 119315717514047 in
  let times = 101741582076661 in
  let sh = reverse_shuffle m input |> repeat ~m times in
  get ~m sh 2020

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
