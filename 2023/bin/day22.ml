open Utils
open Utils.Fn.Infix
open Utils.List.Infix

(*
 * Types
 *)
type coord3 = int * int * int (* x,y,z *)
type brick = coord3 * coord3

let getx (x, _, _) = x
let gety (_, y, _) = y
let getz (_, _, z) = z
let minz ((_, _, z1), (_, _, z2)) = min z1 z2
let maxz ((_, _, z1), (_, _, z2)) = max z1 z2

let compare_bricks b1 b2 =
  match compare (minz b1) (minz b2) with 0 -> compare b1 b2 | c -> c

module IntMap = Map.Make (Int)

module BrickMap = Map.Make (struct
  type t = brick

  let compare = compare_bricks
end)

(*
 * Parse input
 *)
let parse_brick (line : string) : brick =
  let parse_coord s =
    let [ x; y; z ] = List.of_string ~sep:',' s in
    (x, y, z)
  in
  let [ left; right ] = String.split_on_char '~' line in
  (parse_coord left, parse_coord right)

let puzzle_input = Io.read_lines "data/22.txt" ||> parse_brick

(*
 * Part 1
 *)

let shadow (((x1, y1, _), (x2, y2, _)) : brick) : Coord.t list =
  if x1 = x2 then List.irange ~from:y1 y2 ||> Pair.pack x1
  else List.irange ~from:x1 x2 ||> Pair.rpack y1

let shadow_heights (b : brick) : (Coord.t * int) Seq.t =
  shadow b ||> Pair.rpack (maxz b) |> List.to_seq

let drop_bricks (bricks : brick list) : brick list =
  let drop_brick ((x1, y1, z1), (x2, y2, z2)) new_z =
    let drop = min z1 z2 - new_z in
    ((x1, y1, z1 - drop), (x2, y2, z2 - drop))
  in
  let accumulate heights brick =
    let max_height c =
      Coord.Map.find_opt c heights |> Option.value ~default:0
    in
    let coords = shadow brick in
    let z' = coords ||> max_height |> List.max |> ( + ) 1 in
    let brick' = drop_brick brick z' in
    let heights' = Coord.Map.add_seq (shadow_heights brick') heights in
    (heights', drop_brick brick z')
  in
  bricks |> List.sort compare_bricks
  |> List.fold_left_map accumulate Coord.Map.empty
  |> Pair.right

let supports_brick (bottom : brick) (top : brick) : bool =
  if maxz bottom + 1 <> minz top then false
  else
    let bottom_cs, top_cs = (shadow bottom, shadow top) in
    List.exists (fun c -> List.exists (( = ) c) bottom_cs) top_cs

let brick_layers (bricks : brick list) : brick list IntMap.t =
  let same_layer b1 b2 = minz b1 = minz b2 in
  let add_layer bs = (minz (List.hd bs), bs) in
  bricks |> List.sort compare_bricks |> List.group same_layer ||> add_layer
  |> IntMap.of_list

let get_edges (bricks : brick list) : (brick * brick) list =
  let layers = brick_layers bricks in
  let bricks_supported_by (b : brick) =
    match IntMap.find_opt (maxz b + 1) layers with
    | None -> []
    | Some bs -> List.filter (fun b2 -> supports_brick b b2) bs ||> Pair.pack b
  in
  List.concat_map bricks_supported_by bricks

let edge_map (edges : (brick * brick) list) : brick list BrickMap.t =
  let should_group (l1, _) (l2, _) = l1 = l2 in
  let build_key es = (Pair.left (List.hd es), List.map Pair.right es) in
  edges |> List.sort compare |> List.group should_group ||> build_key
  |> BrickMap.of_list

let safe_to_disintegrate (bricks : brick list) : int =
  let edges = get_edges bricks in
  let by_bottom = edge_map edges in
  let by_top = edges ||> Pair.swap |> edge_map in
  let safe bottom =
    match BrickMap.find_opt bottom by_bottom with
    | None -> true
    | Some bs ->
        bs
        ||> (fun top -> BrickMap.find top by_top |> List.length |> Fn.gt 1)
        |> List.all
  in
  List.count safe bricks

let part_one = drop_bricks >> safe_to_disintegrate

(*
 * Part 2
 *)
let print_brick ((x1, y1, z1), (x2, y2, z2)) =
  Printf.printf "(%d,%d,%d),(%d,%d,%d)  " x1 y1 z1 x2 y2 z2

let print_bricks bs = List.iter print_brick bs

let count_chain_reactions (bricks : brick list) : int =
  let rec remove_bricks by_bottom by_top bs =
    if List.is_empty bs then 0
    else
      (* Remove the current bricks from the above layers *)
      let above =
        List.filter_map (fun b -> BrickMap.find_opt b by_bottom) bs
        |> List.flatten
        |> List.sort_uniq compare_bricks
      in
      let remove_from_tops acc top =
        let update (Some existing) =
          Option.some (List.filter (fun b -> not (List.mem b bs)) existing)
        in
        BrickMap.update top update acc
      in
      let by_top' = List.fold_left remove_from_tops by_top above in
      (* Find the next set to remove *)
      let should_remove b' = BrickMap.find b' by_top' |> List.is_empty in
      let to_remove = above |> List.filter should_remove in
      (* Recurse and count removed nodes *)
      List.length to_remove + remove_bricks by_bottom by_top' to_remove
  in
  let edges = get_edges bricks in
  let by_bottom = edge_map edges in
  let by_top = edges ||> Pair.swap |> edge_map in
  let remove_brick b = remove_bricks by_bottom by_top [ b ] in
  List.map remove_brick bricks |> List.sum

let part_two = drop_bricks >> count_chain_reactions

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
