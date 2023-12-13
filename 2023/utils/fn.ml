(* Identity function *)
let id x = x

module Infix = struct
  (* Functional composition: *)
  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end
