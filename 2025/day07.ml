open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2025/data/07.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
let trace_beams (manifold : char matrix) : Coord.Set.t =
  let open Coord in
  let tick c =
    let c' = step c Down in
    if Matrix.in_bounds manifold c' then Some c' else None
  in
  let split c (splitters, beams) =
    if Matrix.get manifold c = '^' then
      let splitters' = Set.add c splitters in
      let beams' = (Set.add (step c Left) >> Set.add (step c Right)) beams in
      (splitters', beams')
    else (splitters, Set.add c beams)
  in
  let rec trace splitters beams =
    let beams' = Set.filter_map tick beams in
    if Set.is_empty beams' then splitters
    else
      let splitters', beams' = Set.fold split beams' (splitters, Set.empty) in
      trace splitters' beams'
  in
  let (Some c) = Matrix.find (( = ) 'S') manifold in
  trace Set.empty (Set.singleton c)

let part_one = trace_beams >> Coord.Set.cardinal

(*
 * Part 2
 *)
let count_beams (manifold : char matrix) : int =
  let open Coord in
  let trace self beam =
    let beam' = step beam Down in
    match Matrix.get_opt manifold beam' with
    | Some '^' -> (step beam' Left |> self) + (step beam' Right |> self)
    | Some _ -> self beam'
    | None -> 1
  in
  let (Some c) = Matrix.find (( = ) 'S') manifold in
  Memo.memo_rec trace c

let part_two = count_beams

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
