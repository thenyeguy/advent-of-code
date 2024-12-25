(* Identity function *)
let id x = x

(* Always returns a constant. *)
let const c _ = c

(* Reverse args (for currying) *)
let swap f r l = f l r

(* Repeatedly apply a function to its own output. *)
let rec repeat (f : 'a -> 'a) (n : int) (x : 'a) : 'a =
  if n = 0 then x else (repeat [@tailcall]) f (n - 1) (f x)

(* Reversed operators (for currying) *)
let lt r l = l < r
let gt r l = l > r
let lte r l = l <= r
let gte r l = l >= r
let sub r l = l - r
let div r l = l / r

module Infix = struct
  (* Functional composition: *)
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end
