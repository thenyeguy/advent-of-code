(* Memoizes a function `f`. *)
let memo (f : 'a -> 'b) : 'a -> 'b =
  let h = Hashtbl.create 128 in
  fun x ->
    match Hashtbl.find_opt h x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add h x y;
        y

(* Memoizes a recursive function `f`.
 * The first argument to f must be itself (so we can inject the memoized
 * version for recursive calls).
 *)
let memo_rec (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
  let h = Hashtbl.create 128 in
  let rec f' x =
    match Hashtbl.find_opt h x with
    | Some y -> y
    | None ->
        let y = f f' x in
        Hashtbl.add h x y;
        y
  in
  f'

let fib (n : int) : int =
  let fib_inner self n =
    match n with 0 -> 0 | 1 -> 1 | _ -> self (n - 1) + self (n - 2)
  in
  memo_rec fib_inner n
