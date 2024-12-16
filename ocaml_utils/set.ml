include Stdlib.Set
include Fn.Infix

module Make (Ord : OrderedType) = struct
  include Stdlib.Set.Make (Ord)

  let contains (s : t) (x : Ord.t) : bool = find_opt x s |> Option.is_some

  let replace (old_x : Ord.t) (new_x : Ord.t) : t -> t =
    remove old_x >> add new_x
end
