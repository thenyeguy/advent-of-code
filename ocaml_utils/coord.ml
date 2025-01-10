(* A coordinate type of (row, col). *)
type t = int * int

(* A directional type. *)
type dir = Up | Right | Down | Left

let dirs = [ Up; Right; Down; Left ]

(* A coordinate difference. *)
type vec = int * int

(* All directions to surrounding coords. *)
let all_direction_vecs =
  [ (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1) ]

(* Make the module orderable. *)
let compare = compare

(* Coordinate containers: *)
module Set = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module Map = Map.Make (struct
  type t = int * int

  let compare = compare
end)

(* Returns the L1 distance between two coords. *)
let manhattan_distance ((r1, c1) : t) ((r2, c2) : t) : int =
  abs (r1 - r2) + abs (c1 - c2)

(* Coordinate math: *)
let add ((x, y) : t) ((dx, dy) : vec) : t = (x + dx, y + dy)
let sub ((x, y) : t) ((dx, dy) : vec) : t = (x - dx, y - dy)

(* Bounds checking *)
let in_bounds ?(lower : t = (0, 0)) ~(upper : t) (c : t) =
  let lx, ly = lower in
  let ux, uy = upper in
  let x, y = c in
  lx <= x && x <= ux && ly <= y && y <= uy

(* Steps a coord in the given direction. *)
let step ?(steps : int = 1) ((row, col) : t) (dir : dir) : t =
  match dir with
  | Up -> (row - steps, col)
  | Right -> (row, col + steps)
  | Down -> (row + steps, col)
  | Left -> (row, col - steps)

let flip (d : dir) : dir =
  match d with Up -> Down | Down -> Up | Left -> Right | Right -> Left

let turn_left (d : dir) : dir =
  match d with Up -> Left | Left -> Down | Down -> Right | Right -> Up

let turn_right (d : dir) : dir =
  match d with Up -> Right | Right -> Down | Down -> Left | Left -> Up

(* Returns all coordinates orthogonally adjacent to the coordinate. *)
let adjacencies (c : t) : t list = List.map (step c) [ Up; Right; Down; Left ]

(* Returns all coordinates surrounding the coordinate. *)
let surrounding (c : t) : t list = List.map (add c) all_direction_vecs

(* Returns the direction orthogonal to d. *)
let orthogonal_dirs (d : dir) : dir list =
  match d with Up | Down -> [ Left; Right ] | Left | Right -> [ Up; Down ]

(* String parsing *)
let coord_of_string (s : string) : t = Scanf.sscanf s "%d,%d" Pair.pack
let string_of_coord ((r, c) : t) : string = Format.sprintf "(%d,%d)" r c

let dir_of_char (c : char) : dir =
  match c with
  | 'U' -> Up
  | 'L' -> Left
  | 'R' -> Right
  | 'D' -> Down
  | _ -> raise (Failure "coord_of_string")

module Infix = struct
  (* Coordinate math: *)
  let ( ++ ) : t -> vec -> t = add
  let ( -- ) : t -> vec -> t = sub
end
