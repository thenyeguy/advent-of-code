let left (l, _) = l
let right (_, r) = r
let pack l r = (l, r)
let apply f (l, r) = f l r
