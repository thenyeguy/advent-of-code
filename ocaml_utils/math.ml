(* Greatest common denominator of a and b *)
let rec gcd (a : int) (b : int) : int = if b = 0 then a else gcd b (a mod b)

(* Least common multiple of a and b *)
let lcm (a : int) (b : int) : int = a * b / gcd a b

(* Modulus operator that always returns a value in the range [0,b) *)
let pos_mod (a : int) (b : int) : int =
  let m = a mod b in
  if m < 0 then m + b else m

module Infix = struct
  let ( % ) = ( mod )
  let ( %~ ) = pos_mod
end
