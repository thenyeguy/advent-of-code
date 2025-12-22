open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/12.txt"

(*
 * Part 1
 *)
type region = { width : int; height : int; num_shapes : int list }
type input = { shapes : char matrix list; regions : region list }

let parse (lines : string list) : input =
  let parse_shape lines = (List.drop 1 >> Matrix.of_strings) lines in
  let parse_region line =
    Scanf.sscanf line "%dx%d: %d %d %d %d %d %d"
      (fun width height a b c d e f ->
        { width; height; num_shapes = [ a; b; c; d; e; f ] })
  in
  let blocks = Io.split_blocks lines in
  let shapes = List.take 6 blocks ||> parse_shape in
  let regions = List.last blocks ||> parse_region in
  { shapes; regions }

let trivially_fits (region : region) : bool =
  let cells = region.width / 3 * (region.height / 3) in
  List.sum region.num_shapes <= cells

let trivially_does_not_fit (shapes : char matrix list) (region : region) : bool
    =
  let cells = region.width * region.height in
  let shape_cells = List.map (Matrix.count (( = ) '#')) shapes in
  let total_cells = List.map2 ( * ) region.num_shapes shape_cells |> List.sum in
  total_cells > cells

let count_fits (input : input) : int =
  let exception Ambiguous in
  let fits r =
    if trivially_fits r then true
    else if trivially_does_not_fit input.shapes r then false
    else raise Ambiguous
  in
  List.count fits input.regions

let part_one = parse >> count_fits

(*
 * Part 2
 *)
let part_two = Runner.unimplemented

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
