open Utils
open Utils.List.Infix

(*
 * Types
 *)
type coord = int * int (* row,col *)
type dir = Up | Down | Left | Right
type map = int Matrix.t

module Search = Search.Make (struct
  type t = coord * dir

  let compare = compare
end)

let orthogonals (d : dir) : dir list =
  if d = Up || d = Down then [ Left; Right ] else [ Up; Down ]

(*
 * Parse input
 *)
let int_of_digit (c : char) : int =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> raise (Failure "int_of_digit")

let puzzle_input =
  Io.read_lines "data/17.txt" |> Matrix.of_strings |> Matrix.map int_of_digit

(*
 * Part 1
 *)
let neighbors ~(min_steps : int) ~(max_steps : int) (map : map)
    ((coord, dir) : Search.node) : Search.node list =
  let steps dir =
    let step n =
      let r, c = coord in
      match dir with
      | Up -> ((r - n, c), dir)
      | Down -> ((r + n, c), dir)
      | Left -> ((r, c - n), dir)
      | Right -> ((r, c + n), dir)
    in
    List.range ~from:min_steps (max_steps + 1) ||> step
  in
  dir |> orthogonals |> List.concat_map steps
  |> List.filter (fun ((r, c), _) -> Matrix.in_bounds map r c)

let get_cost (map : map) ((src, _) : Search.node) ((dest, dir) : Search.node) :
    int =
  let (rs, cs), (rd, cd) = (src, dest) in
  let steps =
    match dir with
    | Up -> List.irange ~from:(rs - 1) rd ||> fun r -> (r, cd)
    | Down -> List.irange ~from:(rs + 1) rd ||> fun r -> (r, cd)
    | Left -> List.irange ~from:(cs - 1) cd ||> fun c -> (rd, c)
    | Right -> List.irange ~from:(cs + 1) cd ||> fun c -> (rd, c)
  in
  steps ||> Pair.apply (Matrix.get map) |> List.sum

let is_done (map : map) ((coord, _) : Search.node) : bool =
  let dest = (Matrix.rows map - 1, Matrix.cols map - 1) in
  coord = dest

let find_shortest_path ?(min_steps : int = 1) ~(max_steps : int) (map : map) =
  Search.find_shortest_path
    (neighbors ~min_steps ~max_steps map)
    (get_cost map) (is_done map)
    [ ((0, 0), Right); ((0, 0), Down) ]

let part_one (map : map) : int = find_shortest_path ~max_steps:3 map

(*
 * Part 2
 *)
let part_two (map : map) : int =
  find_shortest_path ~min_steps:4 ~max_steps:10 map

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
