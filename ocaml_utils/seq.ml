include Stdlib.Seq

let range ?(from : int = 0) (to_ : int) : int t =
  if from < to_ then init (to_ - from) (fun i -> from + i)
  else init (from - to_) (fun i -> from - i)
