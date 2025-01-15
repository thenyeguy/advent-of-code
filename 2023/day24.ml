open Utils

(*
 * Parse input
 *)
type vec2 = float * float
type vec3 = float * float * float
type hailstone = { pos : vec3; velocity : vec3 }

(*
 * Parse input
 *)
let parse_hailstone (line : string) : hailstone =
  let parse_vec3 s =
    let [ x; y; z ] = List.of_string ~sep:',' s ||> float_of_int in
    (x, y, z)
  in
  let [ pos; velocity ] = String.split_on_char '@' line ||> parse_vec3 in
  { pos; velocity }

let puzzle_input = Io.read_lines "2023/data/24.txt" ||> parse_hailstone

(*
 * Part 1
 *)
let path_intersection (h1 : hailstone) (h2 : hailstone) : vec2 option =
  let (x1, y1, _), (dx1, dy1, _) = (h1.pos, h1.velocity) in
  let x2, y2 = (x1 +. dx1, y1 +. dy1) in
  let (x3, y3, _), (dx2, dy2, _) = (h2.pos, h2.velocity) in
  let x4, y4 = (x3 +. dx2, y3 +. dy2) in
  let d = ((x1 -. x2) *. (y3 -. y4)) -. ((y1 -. y2) *. (x3 -. x4)) in
  if d = 0. then None
  else
    let t = (((x1 -. x3) *. (y3 -. y4)) -. ((y1 -. y3) *. (x3 -. x4))) /. d in
    let u = (((x1 -. x3) *. (y1 -. y2)) -. ((y1 -. y3) *. (x1 -. x2))) /. d in
    if t < 0. || u < 0. then None else Some (x1 +. (t *. dx1), y1 +. (t *. dy1))

let in_region (lower : float) (upper : float) ((x, y) : vec2) : bool =
  lower <= x && x <= upper && lower <= y && y <= upper

let count_intersections (lower : float) (upper : float) (hs : hailstone list) :
    int =
  List.combinations hs
  |> List.filter_map (Fn.uncurry path_intersection)
  |> List.count (in_region lower upper)

let part_one = count_intersections 200000000000000. 400000000000000.

(*
 * Part 2
 *
 * Given:
 *   pn = position of hailstone n
 *   vn = velocity of hailstone n
 *   tn = time of hailstone n
 * We solve the system of non-linear equations:
 *   pn + tn * vn = ps + tn * vs      (rearranged as:)
 *   (ps - pn) + tn * (vs - vn)  = 0
 * For ps, vs and all tn. This requires 3 vector equations, and thus three hailstones.
 *
 * We find this solution numerically using Newton's method.
 *)
let f (h1 : hailstone) (h2 : hailstone) (h3 : hailstone)
    (solution : float array) : float array =
  let [| xs; ys; zs; dxs; dys; dzs; t1; t2; t3 |] = solution in
  let f' (h : hailstone) (t : float) =
    let (x, y, z), (dx, dy, dz) = (h.pos, h.velocity) in
    [|
      xs -. x +. ((dxs -. dx) *. t);
      ys -. y +. ((dys -. dy) *. t);
      zs -. z +. ((dzs -. dz) *. t);
    |]
  in
  Array.concat [ f' h1 t1; f' h2 t2; f' h3 t3 ]

let jacobian (h1 : hailstone) (h2 : hailstone) (h3 : hailstone)
    (x : float array) : float matrix =
  let dx1, dy1, dz1 = h1.velocity in
  let dx2, dy2, dz2 = h2.velocity in
  let dx3, dy3, dz3 = h3.velocity in
  let [| _; _; _; dxs; dys; dzs; t1; t2; t3 |] = x in
  [|
    [| 1.; 0.; 0.; t1; 0.; 0.; dxs -. dx1; 0.; 0. |];
    [| 0.; 1.; 0.; 0.; t1; 0.; dys -. dy1; 0.; 0. |];
    [| 0.; 0.; 1.; 0.; 0.; t1; dzs -. dz1; 0.; 0. |];
    [| 1.; 0.; 0.; t2; 0.; 0.; 0.; dxs -. dx2; 0. |];
    [| 0.; 1.; 0.; 0.; t2; 0.; 0.; dys -. dy2; 0. |];
    [| 0.; 0.; 1.; 0.; 0.; t2; 0.; dzs -. dz2; 0. |];
    [| 1.; 0.; 0.; t3; 0.; 0.; 0.; 0.; dxs -. dx3 |];
    [| 0.; 1.; 0.; 0.; t3; 0.; 0.; 0.; dys -. dy3 |];
    [| 0.; 0.; 1.; 0.; 0.; t3; 0.; 0.; dzs -. dz3 |];
  |]

let find_rock (hailstones : hailstone list) : hailstone =
  let (h1 :: h2 :: h3 :: _) = hailstones in
  let guess = [| 10.; 10.; 10.; 10.; 10.; 10.; 10.; 20.; 30. |] in
  let solution =
    Lin_alg.newton_estimate (f h1 h2 h3) (jacobian h1 h2 h3) guess
  in
  let [| x; y; z; dx; dy; dz; _; _; _ |] = solution in
  { pos = (y, x, z); velocity = (dx, dy, dz) }

let part_two (input : hailstone list) : int =
  let rock = find_rock input in
  let x, y, z = rock.pos in
  [ x; y; z ] ||> Float.round ||> int_of_float |> List.sum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
