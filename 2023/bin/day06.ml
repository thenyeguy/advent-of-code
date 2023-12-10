open Utils
open Utils.Infix

type race = { time : int; distance : int }

let test_input =
  [
    { time = 7; distance = 9 };
    { time = 15; distance = 40 };
    { time = 30; distance = 200 };
  ]

let puzzle_input =
  [
    { time = 40; distance = 277 };
    { time = 82; distance = 1338 };
    { time = 91; distance = 1349 };
    { time = 66; distance = 1063 };
  ]

let solve_quadratic (a : float) (b : float) (c : float) : float * float =
  (* (-b +/- sqrt(b^2 - 4ac)) / 2a *)
  let bb = b *. b in
  let a2 = 2. *. a in
  let arg = sqrt ((b *. b) -. (4. *. a *. c)) in
  ((bb +. arg) /. a2, (bb -. arg) /. a2)

let wins_race (r : race) (wait : int) : bool =
  (* wait^2 - length*wait + distance > 0 *)
  let race_distance length wait = wait * (length - wait) in
  race_distance r.time wait > r.distance

let ways_to_win_race (r : race) : int =
  let upper, lower =
    solve_quadratic 1. (float_of_int (-r.time)) (float_of_int r.distance)
  in
  truncate upper - truncate lower

let part_one (races : race list) = races ||> ways_to_win_race |> List.product

let part_two _ =
  ways_to_win_race { time = 40_82_91_66; distance = 277_1338_1349_1063 }

let _ = Runner.main puzzle_input part_one part_two
