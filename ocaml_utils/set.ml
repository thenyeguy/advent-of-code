include Stdlib.Set

module Make (Ord : OrderedType) = struct
  include Stdlib.Set.Make (Ord)

  let contains (s : t) (x : Ord.t) : bool = find_opt x s |> Option.is_some
end
