(* Identity function *)
let id x = x

(* Splits a list of lines into blocks, seperated by empty lines. *)
let split_blocks (input : string list) : string list list =
  let accumulate block line =
    match line with
    | "" -> ([], Option.some block)
    | _ -> (List.append block [ line ], Option.none)
  in
  let block, blocks = List.fold_left_map accumulate [] input in
  List.append (List.filter_map id blocks) [ block ]
