(* Greatest common denominator of a and b *)
let rec gcd (a : int) (b : int) : int = if b = 0 then a else gcd b (a mod b)

(* Least common multiple of a and b *)
let lcm (a : int) (b : int) : int = a * b / gcd a b
