(* Recommended usage is to [open Utils] at the start of any new file.
 * This will shadow all the stdlib modules we are extending.
 *)

(* Import all modules. *)
module Char = Char
module Coord = Coord
module Fn = Fn
module Io = Io
module Lin_alg = Lin_alg
module List = List
module Map = Map
module Matrix = Matrix
module Memo = Memo
module Option = Option
module Pair = Pair
module Runner = Runner
module Search = Search
module Set = Set
module String = String

(* Automatically export a few common operators when opening. *)
let ( << ) = Fn.Infix.( << )
let ( >> ) = Fn.Infix.( >> )
let ( ||> ) = List.Infix.( ||> )
