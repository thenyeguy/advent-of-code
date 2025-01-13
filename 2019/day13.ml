open Utils

(*
 * Parse input
 *)
let puzzle_input = Intcode.Io.read_program "2019/data/13.txt"

(*
 * Part 1
 *)
type tile_t = Empty | Wall | Block | Paddle | Ball
type state_t = { screen : tile_t matrix; mutable score : int }

let new_state () = { screen = Matrix.make 20 38 Empty; score = 0 }

let tile_of_int (i : int) : tile_t =
  match i with 0 -> Empty | 1 -> Wall | 2 -> Block | 3 -> Paddle | 4 -> Ball

let update (c : Intcode.Computer.t) (state : state_t) =
  let open Intcode.Computer in
  while has_output c do
    let x = pop c in
    let y = pop c in
    let tile = pop c in
    if x = -1 then state.score <- tile
    else Matrix.set state.screen (y, x) (tile_of_int tile)
  done

let count_blocks (state : state_t) : int =
  let acc count tile = if tile = Block then count + 1 else count in
  Matrix.fold acc 0 state.screen

let part_one input =
  let open Intcode.Computer in
  let c = init input |> run in
  let state = new_state () in
  update c state;
  count_blocks state

(*
 * Part 2
 *)
let draw_screen (state : state_t) : string =
  let draw tile =
    match tile with
    | Empty -> ' '
    | Wall -> '#'
    | Block -> 'x'
    | Paddle -> '-'
    | Ball -> 'o'
  in
  Matrix.map draw state.screen |> Matrix.to_string

let paddle_dir (state : state_t) : int =
  let (Some (_, paddle_x)) = Matrix.find (( = ) Paddle) state.screen in
  let (Some (_, ball_x)) = Matrix.find (( = ) Ball) state.screen in
  (* Compare will return [-1,1] to move towards the ball. *)
  compare ball_x paddle_x

let play_breakout (prog : Intcode.Computer.program_t) : int =
  let open Intcode.Computer in
  let prog_with_quarters = 2 :: List.tl prog in
  let c = init prog_with_quarters in
  let state = new_state () in
  while is_running c do
    ignore (run c);
    update c state;
    push c (paddle_dir state)
  done;
  state.score

let part_two = play_breakout

(*
 * Main
 *)
let _ = Runner.main puzzle_input part_one part_two
