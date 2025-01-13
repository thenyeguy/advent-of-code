open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2024/data/22.txt" ||> int_of_string

(*
 * Part 1
 *)
let mix (secret : int) (value : int) : int = Int.logxor secret value
let prune (secret : int) : int = secret mod 16777216
let lshft = Fn.swap Int.shift_left
let rshft = Fn.swap Int.shift_right

let next (secret : int) : int =
  let tick f s = mix s (f s) |> prune in
  secret |> tick (lshft 6) |> tick (rshft 5) |> tick (lshft 11)

let part_one input = input ||> Fn.repeat next 2000 |> List.sum

(*
 * Part 2
 *)
module WindowMap = Map.Make (struct
  type t = int * int * int * int

  let compare = compare
end)

let prices ?(n : int = 2000) (secret : int) : int list =
  let acc s _ =
    let s' = next s in
    (s', s mod 10)
  in
  List.fold_left_map acc secret (List.range n) |> Pair.right

let rec sales (prices : int list) : int WindowMap.t =
  match prices with
  | p1 :: p2 :: p3 :: p4 :: p5 :: _ ->
      let window = (p2 - p1, p3 - p2, p4 - p3, p5 - p4) in
      WindowMap.add window p5 (sales (List.tl prices))
  | _ -> WindowMap.empty

let total_sales (sales : int WindowMap.t list) : WindowMap.key * int =
  let add _ l r = Option.merge ( + ) l r in
  let max w v (max_w, max_v) = if v > max_v then (w, v) else (max_w, max_v) in
  let totals = List.fold_left (WindowMap.merge add) WindowMap.empty sales in
  WindowMap.fold max totals ((0, 0, 0, 0), 0)

let part_two input = input ||> prices ||> sales |> total_sales |> Pair.right

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
