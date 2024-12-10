open Utils
open Utils.Fn.Infix
open Utils.List.Infix

(*
 * Parse input
 *)
let puzzle_input = Io.read_file "2024/data/09.txt"

(*
 * Part 1
 *)
type file = { id : int; size : int }

(* Converts a disc map to a list of files, with id=-1 representing empty. *)
let init_files (map : string) : file list =
  let rec init id bs =
    match bs with
    | b :: e :: bs' ->
        { id; size = b } :: { id = -1; size = e } :: init (id + 1) bs'
    | [ b ] -> [ { id; size = b } ]
  in
  String.explode map ||> Char.digit_of_char |> init 0

(* Gets the number of non-empty blocks. *)
let file_length (files : file list) : int =
  let file_size f = if f.id >= 0 then Some f.size else None in
  List.filter_map file_size files |> List.sum

(* Takes n blocks from the front of files, returning (popped, rest). *)
let rec pop (n : int) (files : file list) : file list * file list =
  match files with
  | { id = -1; _ } :: files' -> pop n files'
  | { id; size } :: files' when size > n ->
      ([ { id; size = n } ], { id; size = size - n } :: files')
  | { id; size } :: files' when size = n -> ([ { id; size = n } ], files')
  | { id; size } :: files' ->
      let blocks, rest = pop (n - size) files' in
      ({ id; size } :: blocks, rest)

(* Files empty files from the back. *)
let repack_blocks (files : file list) : file list =
  let rec repack (front : file list) (back : file list) : file list =
    match front with
    | [] -> []
    | f :: front' ->
        if f.id >= 0 then f :: repack front' back
        else
          let fill, back' = pop f.size back in
          fill @ repack front' back'
  in
  (* We are going to fill past the middle point, so just naively truncate *)
  repack files (List.rev files) |> pop (file_length files) |> Pair.left

let checksum (files : file list) : int =
  let rec checksum' files i =
    match files with
    | [] -> 0
    | { id = -1; size } :: files' -> checksum' files' (i + size)
    | { size; _ } :: files' when size = 0 -> checksum' files' i
    | { id; size } :: files' ->
        (i * id) + checksum' ({ id; size = size - 1 } :: files') (i + 1)
  in
  checksum' files 0

let part_one = init_files >> repack_blocks >> checksum

(*
 * Part 2
 *)
let repack_files (files : file list) : file list =
  let rec erase_file (target : int) (fs : file list) : file list =
    match fs with
    | { id = -1; size = lsize }
      :: { id; size }
      :: { id = -1; size = rsize }
      :: fs'
      when id = target ->
        { id = -1; size = lsize + size + rsize } :: fs'
    | { id = -1; size = lsize } :: { id; size } :: fs' when id = target ->
        { id = -1; size = lsize + size } :: fs'
    | { id; size } :: { id = -1; size = rsize } :: fs' when id = target ->
        { id = -1; size = size + rsize } :: fs'
    | { id; size } :: fs' when id = target -> { id = -1; size } :: fs'
    | f :: fs' -> f :: erase_file target fs'
  in
  let rec repack_file (fs : file list) (f : file) =
    match fs with
    | { id; _ } :: fs' when id = f.id -> f :: fs'
    | { id = -1; size } :: fs' when size = f.size -> f :: erase_file f.id fs'
    | { id = -1; size } :: fs' when size > f.size ->
        f :: { id = -1; size = size - f.size } :: erase_file f.id fs'
    | f' :: fs' -> f' :: repack_file fs' f
  in
  List.filter (fun f -> f.id >= 0) files
  |> List.rev
  |> List.fold_left repack_file files

let part_two = init_files >> repack_files >> checksum

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
