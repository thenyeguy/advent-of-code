include Stdlib.Option

let or_ (a : 'a option) (b : 'a option) : 'a option = if is_some a then a else b
