(* List.map operator: *)
let ( ||> ) (l : 'a list) (f : 'a -> 'b) : 'b list = List.map f l

(* Functional composition: *)
let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
