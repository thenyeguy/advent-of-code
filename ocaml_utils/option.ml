include Stdlib.Option

let or_ (a : 'a option) (b : 'a option) : 'a option = if is_some a then a else b

let merge (f : 'a -> 'a -> 'a) (a : 'a option) (b : 'a option) : 'a option =
  match (a, b) with
  | Some a, Some b -> Some (f a b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None

let rbind (f : 'a -> 'b option) (a : 'a option) : 'b option = bind a f
