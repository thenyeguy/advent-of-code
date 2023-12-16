(* Identity function *)
let id x = x

(* Always returns a constant. *)
let const c _ = c

module Infix = struct
  (* Functional composition: *)
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end
