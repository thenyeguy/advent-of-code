(* Solves the vector equation A*x=b for x using gaussian elimination. *)
let solve (a : float Matrix.t) (b : float array) : float array =
  let rows, cols = Matrix.size a in
  let m = Array.map2 Array.append a (Array.map (fun n -> [| n |]) b) in
  let zero_out row col =
    assert (abs_float m.(col).(col) > 0.);
    let scale = m.(row).(col) /. m.(col).(col) in
    let sub c v = v -. (scale *. m.(col).(c)) in
    Array.mapi_inplace sub m.(row)
  in
  (* Forward elimination: convert to row-echelon form. *)
  for row = 1 to rows - 1 do
    for col = 0 to row - 1 do
      zero_out row col
    done
  done;
  (* Back substitution: convert to trianglar form. *)
  for row = rows - 1 downto 0 do
    for col = cols - 1 downto row + 1 do
      zero_out row col
    done
  done;
  (* Extract x from the final column. *)
  let solve' i row = Array.get row cols /. Array.get row i in
  Array.mapi solve' m

(* Numerically finds a solution to a system of equations f, given:
 *  - f: a function that evaluates the equations at a given point
 *  - jacobian: a function returning the jacobian matrix of partial derivaties
 *              at a given point
 *  - x: an initial guess for the final solution
 *)
let rec newton_estimate (f : float array -> float array)
    (jacobian : float array -> float Matrix.t) (x : float array) : float array =
  let dx = solve (jacobian x) (f x) in
  let x' = Array.map2 ( -. ) x dx in
  let error = Array.fold_left (fun acc n -> acc +. (n *. n)) 0. dx in
  if Float.is_nan error then raise (Failure "nan")
  else if error < 0.1 then x'
  else newton_estimate f jacobian x'
