open Utils

(*
 * Parse input
 *)
let puzzle_input = Io.read_lines "2019/data/24.txt" |> Matrix.of_strings

(*
 * Part 1
 *)
module StringSet = Set.Make (String)

let becomes_bug (is_bug : bool) (adjacent_bugs : int) : bool =
  adjacent_bugs = 1 || ((not is_bug) && adjacent_bugs = 2)

let adjacent_bugs (m : char matrix) (c : coord) : int =
  List.count (( = ) '#') (Matrix.adjacencies m c)

let tick_space (m : char matrix) (row : int) (col : int) (bug : char) : char =
  if becomes_bug (bug = '#') (adjacent_bugs m (row, col)) then '#' else '.'

let tick (m : char matrix) : char matrix = Matrix.mapi (tick_space m) m

let find_repeat (m : char matrix) : char matrix =
  let rec find (seen : StringSet.t) (m : char matrix) : char matrix =
    let s = Matrix.to_string m in
    if StringSet.contains seen s then m
    else find (StringSet.add s seen) (tick m)
  in
  find StringSet.empty m

let rate_biodiversity (m : char matrix) : int =
  let acc (total, points) b =
    if b = '#' then (total + points, 2 * points) else (total, 2 * points)
  in
  Matrix.fold acc (0, 1) m |> fst

let part_one = find_repeat >> rate_biodiversity

(*
 * Part 2
 *)
module IntMap = Map.Make (Int)

module RecursiveGrid = struct
  type t = char matrix IntMap.t

  let singleton (m : char matrix) : t = IntMap.singleton 0 m

  let get (grid : t) (l : int) (c : coord) : char =
    assert (c <> (2, 2));
    match IntMap.find_opt l grid with Some m -> Matrix.get m c | None -> '.'

  let adjacent_bugs (grid : t) (l : int) (c : coord) : int =
    let count_at (l : int) (src : coord) (dst : coord) : int =
      let count l c = if get grid l c = '#' then 1 else 0 in
      match dst with
      | -1, _ -> count (l - 1) (1, 2)
      | 5, _ -> count (l - 1) (3, 2)
      | _, -1 -> count (l - 1) (2, 1)
      | _, 5 -> count (l - 1) (2, 3)
      | 2, 2 ->
          let cs =
            match src with
            | 1, _ -> [ (0, 0); (0, 1); (0, 2); (0, 3); (0, 4) ]
            | 3, _ -> [ (4, 0); (4, 1); (4, 2); (4, 3); (4, 4) ]
            | _, 1 -> [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0) ]
            | _, 3 -> [ (0, 4); (1, 4); (2, 4); (3, 4); (4, 4) ]
          in
          cs ||> count (l + 1) |> List.sum
      | _ -> count l dst
    in
    Coord.adjacencies c ||> count_at l c |> List.sum

  let expand (g : t) : t =
    let minl, _ = IntMap.min_binding g in
    let maxl, _ = IntMap.max_binding g in
    let empty = Matrix.make 5 5 '.' in
    g |> IntMap.add (minl - 1) empty |> IntMap.add (maxl + 1) empty

  let tick (g : t) : t =
    let tick_space (l : int) (r : int) (c : int) (bug : char) : char =
      if (r, c) = (2, 2) then '?'
      else if becomes_bug (bug = '#') (adjacent_bugs g l (r, c)) then '#'
      else '.'
    in
    let tick_level (l : int) (m : char matrix) = Matrix.mapi (tick_space l) m in
    IntMap.mapi tick_level (expand g)

  let count_bugs (g : t) : int =
    let count _ m total =
      let count_inner t x = if x = '#' then t + 1 else t in
      total + Matrix.fold count_inner 0 m
    in
    IntMap.fold count g 0
end

let part_two m =
  let open RecursiveGrid in
  singleton m |> Fn.repeat tick 200 |> count_bugs

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
