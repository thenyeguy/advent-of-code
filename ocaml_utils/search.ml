(* Input signature for the Search functor. *)
module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

(* A lightweight priority queue with integer weights. *)
module PQueue = struct
  module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

  type 'a t = 'a list IntMap.t

  (* The empty priority queue. *)
  let empty = IntMap.empty

  (* Creates a new queue containing a single values. *)
  let singleton ?(priority : int = 0) (value : 'a) : 'a t =
    IntMap.singleton priority [ value ]

  (* Creates a new queue containing all the provided values.
   * Added values will all star twith the same priority
   *)
  let of_values ?(priority : int = 0) (values : 'a list) : 'a t =
    IntMap.singleton priority values

  (* Returns an updated queue containing the new value. *)
  let push (q : 'a t) (value : 'a) (priority : int) : 'a t =
    IntMap.add_to_list priority value q

  (* Removes the value with the lowest priority from the queue.
   * Returns (updated queue, value, priority).
   *)
  let pop (q : 'a t) : ('a t * 'a * int) option =
    let update' values _ =
      if List.is_empty values then Option.none else Option.some values
    in
    match IntMap.min_binding_opt q with
    | Some (priority, value :: values) ->
        let q' = IntMap.update priority (update' values) q in
        Option.some (q', value, priority)
    | None -> Option.none
    | _ -> raise (Failure "PQueue.pop")
end

(* Initialize a search module for the provided node type. *)
module Make (Node : OrderedType) = struct
  type node = Node.t

  module HistorySet = Set.Make (Node)
  module HistoryMap = Map.Make (Node)

  (* Finds the minimum cost path through a given graph, given:
   *  - neighbors: a function from a node to its adjacent nodes
   *  - cost src dest: a function that returns the cost of a given edge
   *  - is_done: a function indicating if the given node is an end state
   *  - starts: a list of nodes to start the search
   *)
  let find_shortest_path (neighbors : node -> node list)
      (cost : node -> node -> int) (is_done : node -> bool) (starts : node list)
      : int =
    let rec dfs (q : node PQueue.t) (seen : HistorySet.t) : int =
      match PQueue.pop q with
      | None -> raise (Failure "find_shortest_path: no shortest path")
      | Some (q', node, priority) ->
          if HistorySet.mem node seen then (dfs [@tailcall]) q' seen
          else if is_done node then priority
          else
            let push q node' =
              PQueue.push q node' (priority + cost node node')
            in
            let q'' = node |> neighbors |> List.fold_left push q' in
            let seen' = HistorySet.add node seen in
            (dfs [@tailcall]) q'' seen'
    in
    dfs (PQueue.of_values starts) HistorySet.empty

  (* Finds the distance from a given node to all other reachable nodes, given:
   *  - neighbors: a function from a node to its adjacent nodes
   *  - cost src dest: a function that returns the cost of a given edge
   *  - starts: a list of nodes to start the search
   *)
  let find_distance_from (neighbors : node -> node list)
      (cost : node -> node -> int) (starts : node list) : int HistoryMap.t =
    let rec dfs (q : node PQueue.t) (seen : int HistoryMap.t) : int HistoryMap.t
        =
      match PQueue.pop q with
      | None -> seen
      | Some (q', node, priority) ->
          if HistoryMap.contains seen node then (dfs [@tailcall]) q' seen
          else
            let seen' = HistoryMap.add node priority seen in
            let push q node' =
              let priority' = priority + cost node node' in
              PQueue.push q node' priority'
            in
            let q'' = node |> neighbors |> List.fold_left push q' in
            (dfs [@tailcall]) q'' seen'
    in
    dfs (PQueue.of_values starts) HistoryMap.empty

  (* Finds the maximum cost path through a given graph, given:
   *  - neighbors: a function from a node to its adjacent nodes
   *  - cost src dest: a function that returns the cost of a given edge
   *  - is_done: a function indicating if the given node is an end state
   *  - starts: a list of nodes to start the search
   *)
  let find_longest_path (neighbors : node -> node list)
      (get_cost : node -> node -> int) (is_done : node -> bool) (start : node) :
      int =
    let open List.Infix in
    let rec get_path' (seen : HistorySet.t) (cost : int) (node : node) : int =
      if is_done node then cost
      else
        let seen' = HistorySet.add node seen in
        let is_unseen n = HistorySet.mem n seen |> not in
        let get_path'' node' =
          get_path' seen' (cost + get_cost node node') node'
        in
        neighbors node |> List.filter is_unseen ||> get_path'' |> List.max
    in
    get_path' HistorySet.empty 0 start
end
