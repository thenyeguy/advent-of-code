open Utils

(*
 * Parse input
 *)
let int_of_digit (c : char) : int =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> raise (Failure "int_of_digit")

let puzzle_input =
  Io.read_lines "2023/data/17.txt"
  |> Matrix.of_strings |> Matrix.map int_of_digit

(*
 * Part 1
 *)
module Graph = struct
  type t = int * int * int matrix
  type node = coord * dir

  let neighbors ((min_steps, max_steps, map) : t) ((coord, dir) : node) :
      node list =
    let steps dir =
      let step n = (Coord.step ~steps:n coord dir, dir) in
      List.irange ~from:min_steps max_steps ||> step
    in
    Coord.orthogonal_dirs dir |> List.concat_map steps
    |> List.filter (fun (c, _) -> Matrix.in_bounds map c)

  let cost ((_, _, map) : t) ((src, _) : node) ((dest, dir) : node) : int =
    let (rs, cs), (rd, cd) = (src, dest) in
    let steps =
      match dir with
      | Up -> List.irange ~from:(rs - 1) rd ||> fun r -> (r, cd)
      | Down -> List.irange ~from:(rs + 1) rd ||> fun r -> (r, cd)
      | Left -> List.irange ~from:(cs - 1) cd ||> fun c -> (rd, c)
      | Right -> List.irange ~from:(cs + 1) cd ||> fun c -> (rd, c)
    in
    steps ||> Matrix.get map |> List.sum

  let is_done ((_, _, map) : t) ((coord, _) : node) : bool =
    let dest = (Matrix.rows map - 1, Matrix.cols map - 1) in
    coord = dest
end

module Search = Search.Make (Graph)

let find_shortest_path ?(min_steps : int = 1) ~(max_steps : int)
    (map : int matrix) =
  Search.find_shortest_path
    (min_steps, max_steps, map)
    [ ((0, 0), Right); ((0, 0), Down) ]

let part_one (map : int matrix) : int = find_shortest_path ~max_steps:3 map

(*
 * Part 2
 *)
let part_two (map : int matrix) : int =
  find_shortest_path ~min_steps:4 ~max_steps:10 map

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
