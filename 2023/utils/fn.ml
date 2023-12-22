(* Identity function *)
let id x = x

(* Always returns a constant. *)
let const c _ = c

(* Reversed operators (for currying) *)
let lt r l = l < r
let gt r l = l > r
let lte r l = l <= r
let gte r l = l >= r

module Infix = struct
  (* Functional composition: *)
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end
