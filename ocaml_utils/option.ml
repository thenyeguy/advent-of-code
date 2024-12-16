include Stdlib.Option

let or_ (a : 'a option) (b : 'a option) : 'a option = if is_some a then a else b

let then_ (f : 'a -> 'b option) (a : 'a option) : 'b option =
  match a with Some x -> f x | None -> None
