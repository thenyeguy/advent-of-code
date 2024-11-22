let left (l, _) = l
let right (_, r) = r
let pack l r = (l, r)
let rpack r l = (l, r)
let apply f (l, r) = f l r
let swap (l, r) = (r, l)
