include Stdlib.Map

module Make (Ord : OrderedType) = struct
  include Stdlib.Map.Make (Ord)

  (* Increments the value of a given key in the map. *)
  let increment ?(count : int = 1) (map : int t) (x : Ord.t) =
    let increment' (v : int option) =
      match v with Some i -> Some (i + count) | None -> Some count
    in
    update x increment' map

  (* Returns a map of how many times each element occurs in the given list. *)
  let counts (xs : Ord.t list) : int t = List.fold_left increment empty xs
end
