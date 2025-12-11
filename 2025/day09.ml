open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/09.txt" ||> Coord.coord_of_string

(*
 * Part 1
 *)
let area (((x1, y1), (x2, y2)) : coord * coord) : int =
  (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let part_one = List.combinations >> List.map area >> List.max

(*
 * Part 2
 *)
module IntMap = Map.Make (Int)

type map = int IntMap.t
type polygon = { xs : map; ys : map; map : char Matrix.t }

let edges (cs : coord list) : (coord * coord) list =
  let starts = List.to_seq cs in
  let ends = starts |> Seq.cycle |> Seq.drop 1 in
  Seq.zip starts ends |> List.of_seq

let trace (cs : coord list) : char Matrix.t =
  let rows = cs ||> fst |> List.max in
  let cols = cs ||> snd |> List.max in
  let m = Matrix.make (rows + 2) (cols + 2) '.' in
  let paint ((xl, yl), (xu, yu)) =
    (if xl = xu then
       let cs' = List.irange ~from:yl yu ||> fun y -> (xl, y) in
       List.iter (Fn.flip (Matrix.set m) '-') cs'
     else
       let cs' = List.irange ~from:xl xu ||> fun x -> (x, yl) in
       List.iter (Fn.flip (Matrix.set m) '|') cs');
    Matrix.set m (xl, yl) '#';
    Matrix.set m (xu, yu) '#'
  in
  List.iter paint (edges cs);
  m

let flood_fill (m : char Matrix.t) : char Matrix.t =
  let m = Matrix.copy m in
  let rec flood c =
    if Matrix.get_opt m c = Some '.' then (
      Matrix.set m c ' ';
      List.iter flood (Coord.adjacencies c))
  in
  flood (0, 0);
  m

let compressed_polygon (cs : coord list) : polygon =
  let generate_mapping ns =
    List.sort_uniq compare ns
    |> List.mapi (fun i n -> (n, i + 1))
    |> IntMap.of_list
  in
  let xs = List.map fst cs |> generate_mapping in
  let ys = List.map snd cs |> generate_mapping in
  let compress' (x, y) = (IntMap.find x xs, IntMap.find y ys) in
  let compressed = List.map compress' cs in
  let map = (trace >> flood_fill) compressed in
  { xs; ys; map }

let contains (p : polygon) (((xl, yl), (xu, yu)) : coord * coord) : bool =
  let contains' xl xu yl yu =
    let inside c = Matrix.get p.map c != ' ' in
    let xs = List.irange ~from:xl xu in
    let ys = List.irange ~from:yl yu in
    List.map (Pair.pack xl) ys |> List.all inside
    && List.map (Pair.pack xu) ys |> List.all inside
    && List.map (Pair.rpack yl) xs |> List.all inside
    && List.map (Pair.rpack yu) xs |> List.all inside
  in
  let xl' = IntMap.find xl p.xs in
  let xu' = IntMap.find xu p.xs in
  let yl' = IntMap.find yl p.ys in
  let yu' = IntMap.find yu p.ys in
  contains' xl' xu' yl' yu'

let largest_area (p : polygon) (cs : coord list) : int =
  let rec find rs =
    let ((a, r) :: rs') = rs in
    if contains p r then a else find rs'
  in
  let rs =
    List.combinations cs
    |> List.map (fun r -> (area r, r))
    |> List.sort compare |> List.rev
  in
  find rs

let part_two input = largest_area (compressed_polygon input) input

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
