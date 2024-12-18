(* A coordinate type of (row, col). *)
type t = int * int

(* A  *)
type vec = int * int

(* A directional type. *)
type dir = Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft

let cardinals = [ Up; Right; Down; Left ]
let dirs = [ Up; UpRight; Right; DownRight; Down; DownLeft; Left; UpLeft ]

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

(* Steps a coord in the given direction. *)
let step ?(steps : int = 1) (dir : dir) ((row, col) : t) : t =
  match dir with
  | Up -> (row - steps, col)
  | UpRight -> (row - steps, col + steps)
  | Right -> (row, col + steps)
  | DownRight -> (row + steps, col + steps)
  | Down -> (row + steps, col)
  | DownLeft -> (row + steps, col - steps)
  | Left -> (row, col - steps)
  | UpLeft -> (row - steps, col - steps)

let flip (d : dir) : dir =
  match d with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
  | _ -> raise (Failure "flip")

let turn_left (d : dir) : dir =
  match d with
  | Up -> Left
  | Left -> Down
  | Down -> Right
  | Right -> Up
  | _ -> raise (Failure "turn_left")

let turn_right (d : dir) : dir =
  match d with
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
  | _ -> raise (Failure "turn_right")

(* Returns all coordinates adjacent to the given coordinate. *)
let adjacencies ((row, col) : t) : t list =
  [
    (row - 1, col - 1);
    (row - 1, col);
    (row - 1, col + 1);
    (row, col - 1);
    (row, col + 1);
    (row + 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1);
  ]

(* Returns the direction orthogonal to d. *)
let orthogonals (d : dir) : dir list =
  match d with
  | Up | Down -> [ Left; Right ]
  | Left | Right -> [ Up; Down ]
  | UpLeft | DownRight -> [ UpRight; DownLeft ]
  | UpRight | DownLeft -> [ UpLeft; DownRight ]

module Infix = struct
  (* Coordinate math: *)
  let ( ++ ) : t -> vec -> t = add
  let ( -- ) : t -> vec -> t = sub
end
