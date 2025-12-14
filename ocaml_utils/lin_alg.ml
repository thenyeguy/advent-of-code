(* Augments a linear system of equations [a] with its solutions [b]. *)
let augment (a : float Matrix.t) (b : float array) : float Matrix.t =
  Array.map2 Array.append a (Array.map (fun n -> [| n |]) b)

(* Converts [m] in-place to (reduced) row-echelon form. *)
let to_row_echelon_form (m : float Matrix.t) =
  let swap i j =
    let mi = m.(i) in
    m.(i) <- m.(j);
    m.(j) <- mi
  in
  let scale i s = Array.map_inplace (( *. ) s) m.(i) in
  let add i j s = m.(i) <- Array.map2 (fun a b -> a +. (s *. b)) m.(i) m.(j) in
  let rows, cols = Matrix.size m in
  let row = ref 0 in
  for col = 0 to cols - 2 do
    (* Find a row with the pivot.*)
    let new_pivot = ref !row in
    while !new_pivot < rows && m.(!new_pivot).(col) = 0.0 do
      incr new_pivot
    done;

    (* If we found a pivot, handle it; otherwise, try the next column. *)
    if !new_pivot < rows then begin
      (* Normalize the current row. *)
      swap !row !new_pivot;
      scale !row (1. /. m.(!row).(col));
      (* Zero out the remaining rows. *)
      for row' = 0 to rows - 1 do
        if row' != !row then add row' !row (-1.0 *. m.(row').(col))
      done;
      incr row
    end
  done

(* Solves the vector equation A*x=b for x. Assumes a unique solution exists. *)
let solve (a : float Matrix.t) (b : float array) : float array =
  assert (Matrix.rows a = Matrix.cols a);
  assert (Matrix.rows a = Array.length b);
  let cols = Matrix.cols a in
  let m = augment a b in
  to_row_echelon_form m;
  let solve' i row =
    let s = Array.get row i in
    assert (s > 0.);
    Array.get row cols /. s
  in
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
