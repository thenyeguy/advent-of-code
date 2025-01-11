open Utils

(*
 * vector math
 *)
type vec3 = int * int * int

let ( ++ ) ((xl, yl, zl) : vec3) ((xr, yr, zr) : vec3) =
  (xl + xr, yl + yr, zl + zr)

(*
 * Parse input
 *)
let parse (line : string) : vec3 =
  Scanf.sscanf line "<x=%d, y=%d, z=%d>" (fun x y z -> (x, y, z))

let puzzle_input = Io.read_lines "2019/data/12.txt" ||> parse

(*
 * Part 1
 *)
type axis_t = { pos : int; vel : int }
type moons_t = axis_t list * axis_t list * axis_t list

let unpack (ps : vec3 list) : moons_t =
  let to_axis pos = { pos; vel = 0 } in
  let xs, ys, zs = List.split3 ps in
  (List.map to_axis xs, List.map to_axis ys, List.map to_axis zs)

let apply_gravity (a : axis_t) (b : axis_t) : axis_t =
  { pos = a.pos; vel = a.vel + compare b.pos a.pos }

let step_axis (axes : axis_t list) : axis_t list =
  let step_axis (axis : axis_t) =
    let others = List.filter (( <> ) axis) axes in
    let axis = List.fold_left apply_gravity axis others in
    { axis with pos = axis.pos + axis.vel }
  in
  List.map step_axis axes

let step ((xs, ys, zs) : moons_t) : moons_t =
  (step_axis xs, step_axis ys, step_axis zs)

let energy ((x, y, z) : axis_t * axis_t * axis_t) =
  (abs x.pos + abs y.pos + abs z.pos) * (abs x.vel + abs y.vel + abs z.vel)

let total_energy ((xs, ys, zs) : moons_t) : int =
  List.combine3 xs ys zs ||> energy |> List.sum

let part_one ?(steps = 1000) input =
  input |> unpack |> Fn.repeat step steps |> total_energy

(*
 * Part 2
 *)
module Set = Set.Make (struct
  type t = axis_t list

  let compare = compare
end)

let find_cycle (axes : axis_t list) =
  (* Note that we assume there is no offset before the cycle starts. *)
  let rec find_cycle_inner (seen : Set.t) (axes : axis_t list) (step : int) =
    let axes = step_axis axes in
    if Set.contains seen axes then step
    else
      let seen = Set.add axes seen in
      find_cycle_inner seen axes (step + 1)
  in
  find_cycle_inner Set.empty axes 0

let cycle_length ((xs, ys, zs) : moons_t) : int =
  let xlen = find_cycle xs in
  let ylen = find_cycle ys in
  let zlen = find_cycle zs in
  Math.lcm xlen ylen |> Math.lcm zlen

let part_two = unpack >> cycle_length

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
