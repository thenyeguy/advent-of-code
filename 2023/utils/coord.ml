(* A coordinate type of (row, col). *)
type t = int * int

(* A directional type. *)
type dir = Up | Down | Left | Right

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

(* Steps a coord in the given direction. *)
let step ?(steps : int = 1) (dir : dir) ((row, col) : t) : t =
  match dir with
  | Up -> (row - steps, col)
  | Down -> (row + steps, col)
  | Left -> (row, col - steps)
  | Right -> (row, col + steps)

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
  if d = Up || d = Down then [ Left; Right ] else [ Up; Down ]
