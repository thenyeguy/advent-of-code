include Stdlib.Map

module Make (Ord : OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  (* Returns whether the map contains the given key. *)
  let contains (m : 'a t) (k : Ord.t) : bool = find_opt k m |> Option.is_some

  (* Increments the value of a given key in the map. *)
  let increment ?(count : int = 1) (map : int t) (x : Ord.t) =
    let increment' (v : int option) =
      match v with Some i -> Some (i + count) | None -> Some count
    in
    update x increment' map

  (* Returns a map of how many times each element occurs in the given list. *)
  let counts (xs : Ord.t list) : int t = List.fold_left increment empty xs

  (* Returns the value of [k] in the map, or [default] if [k] is not set. *)
  let find_or (k : key) (default : 'a) (m : 'a t) : 'a =
    match find_opt k m with Some v -> v | None -> default
end
