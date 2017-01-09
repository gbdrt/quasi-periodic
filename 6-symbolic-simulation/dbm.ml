open Format
open Symb
open Bndrel

(* D_i0 = (c, ≺)  means  x_i ≺ c         ; first column is upper bounds       *)
(*                                         (∞, <) means no bound              *)
(* D_0i = (c, ≺)  means  c ≺ x_i         ; first row is lower bounds          *)
(*                                         (0, ≤) means no bound              *)
(* D_ij = (c, ≺)  means  x_i - x_j ≺ c   ; i,j>0, i≠j, bounds on differences  *)
(*                                         (∞, <) means no bound              *)
(* D_ii = (0, ≤)  (tighter than (∞, <))  ; never smaller than (0, ≤)          *)


(** DBMs Creation **)

let zero n = Array.make_matrix (n + 1) (n + 1) b_zero

let one n =
  let r = zero n in
  for i = 0 to n do
    r.(0).(i) <- b_one;
    r.(i).(i) <- b_one
  done;
  r

let make = one

let dims m = Array.length m - 1

(* Used to fix an arbitrary canonical representative for the empty dbm.
 * Note that (x_00 - x_00 < -1) is equivalent to false. *)
let false00 = (Finite (-1), LT)

(* assumes: m1 = cf(m1) *)
let is_empty m1 = (m1.(0).(0) == false00)

(** DBMs Normalization **)

(* Floyd-Warshall for finding the canonical form *)
let cf m =
  let n = dims m in
  try
    for k = 0 to n do
      for i = 0 to n do
        for j = 0 to n do
          let s = m.(i).(k) +* m.(k).(j) in
          if lt_bndrel s m.(i).(j) then begin
            if i = j && s <> b_zero then raise Exit;
            m.(i).(j) <- s
          end
        done
      done
    done
  with Exit -> begin
    for i = 0 to n do
      for j = 0 to n do
        m.(i).(j) <- false00
      done
    done
  end

let mcf m = (cf m; m)

let copy m = Array.map Array.copy m
let fcf m = (let m' = copy m in cf m'; m')


(** DBMs parsing and printing **)

let cond_false = Upper (ClockDiff (0, 0), LT, 0)

let constrain m = function
  | Upper (Clock x, r, b) -> m.(x).(0) <- min_bndrel m.(x).(0) (Finite b, r)
  | Lower (b, r, Clock x) -> m.(0).(x) <- min_bndrel m.(0).(x) (Finite (-b), r)
  | Upper (ClockDiff (x, y), r, b) -> m.(x).(y) <- min_bndrel m.(x).(y) (Finite b, r)
  | Lower (b, r, ClockDiff (x, y)) -> m.(y).(x) <- min_bndrel m.(y).(x) (Finite (-b), r)
  | Both (lb, lr, Clock x, ur, ub) ->
      m.(x).(0) <- min_bndrel m.(x).(0) (Finite ub, ur);
      m.(0).(x) <- min_bndrel m.(0).(x) (Finite (-lb), lr)
  | Both (lb, lr, ClockDiff (x, y), ur, ub) ->
      m.(x).(y) <- min_bndrel m.(x).(y) (Finite ub, ur);
      m.(y).(x) <- min_bndrel m.(y).(x) (Finite (-lb), lr)

(* gives best results when cf(m) = m *)
let constraints m =
  if m.(0).(0) = false00 then [cond_false]
  else
    let cs = ref [] in
    let n = dims m in
    for i = n downto 1 do
      for j = n downto (i + 1) do
        match m.(i).(j), m.(j).(i) with
          (* i - j ≺ b1, j - i ≺ b2 *)
        | (Infinite, _), (Infinite, _) -> ()
        | (Finite b1, r1), (Finite b2, r2) ->
            if b1 >= 0
            then cs := Both (-b2, r2, ClockDiff (i, j), r1, b1) :: !cs
            else cs := Both (-b1, r1, ClockDiff (j, i), r2, b2) :: !cs

        | (Finite b1, r1), _ ->
            if b1 >= 0
            then cs := Upper (ClockDiff (i, j), r1, b1) :: !cs
            else cs := Lower (-b1, r1, ClockDiff (j, i)) :: !cs

        |  _, (Finite b2, r2) ->
            if b2 >= 0
            then cs := Upper (ClockDiff (j, i), r2, b2) :: !cs
            else cs := Lower (-b2, r2, ClockDiff (i, j)) :: !cs
      done
    done;
    for i = n downto 1 do
      match m.(0).(i), m.(i).(0) with
      | (Finite 0, LEQ), (Infinite, LT)
      | (Infinite, _), (Infinite, _) -> ()
      | (Finite 0, LEQ), (Finite ub, ur) ->
          cs := Upper (Clock i, ur, ub) :: !cs
      | (Finite lb, lr), (Finite ub, ur) ->
          cs := Both (-lb, lr, Clock i, ur, ub) :: !cs
      | _, (Finite ub, ur) -> cs := Upper (Clock i, ur, ub) :: !cs
      | (Finite lb, lr), _  -> cs := Lower (-lb, lr, Clock i) :: !cs
    done;
    !cs

let from_string nb_clk s =
  let r = make nb_clk in
  List.iter (constrain r) (Clk.of_string s);
  mcf r


let dump ppf m =
  let pr ppf (b, r) =
    (match b with
     | Infinite -> fprintf ppf "    ∞"
     | Finite i -> fprintf ppf "% 5d" i);
    (match r with
     | LT  -> fprintf ppf "<"
     | LEQ -> fprintf ppf "≤")
  in
  let n = dims m in
  fprintf ppf "@[<v 0>";
  for i = 0 to n do
    fprintf ppf "@[<hov 2>";
    for j = 0 to n do
      if j = 0 then
        fprintf ppf "%a|@," pr m.(i).(j)
      else
        fprintf ppf "%a@ " pr m.(i).(j)
    done;
    fprintf ppf "@]@\n";
    if i = 0 then begin
      fprintf ppf "@[<hov 2>";
      for j = 0 to n do
        if j = 0 then fprintf ppf "   ---+@,"
        else fprintf ppf "-------@,"
      done;
      fprintf ppf "@]@\n";
    end
  done;
  fprintf ppf "@]@."


(* XXX *)
let cf_debug m =
  let n = dims m in
  try
    for k = 0 to n do
      printf "k=%d@\n" k;
      for i = 0 to n do
        for j = 0 to n do
          let s = m.(i).(k) +* m.(k).(j) in
          printf "m(%2d,%2d)=%a > (m(%2d,%2d) + m(%2d,%2d) = %a + %a = %a)@\n"
            i j pr_bndrel_debug m.(i).(j)
            i k
            k j
            pr_bndrel_debug m.(i).(k)
            pr_bndrel_debug m.(k).(j)
            pr_bndrel_debug s;
          if lt_bndrel s m.(i).(j) then begin
            if i = j && s <> b_zero then
              (printf "error!@\n"; raise Exit);
            m.(i).(j) <- s;
            printf "replace (m(%2d,%2d) <- %a@\n%a"
              i j pr_bndrel_debug s dump m
          end
        done
      done
    done; printf "done@\n"
  with Exit -> begin
    for i = 0 to n do
      for j = 0 to n do
        m.(i).(j) <- false00
      done
    done
  end


(** Operations on DBMs **)

let equal m1 m2 = (m1 = m2)

(* Conjunction *)
let (^=) br1 br2 =
  Array.iteri
    (fun i -> Array.iteri
      (fun j v -> br1.(i).(j) <- v ^* br2.(i).(j))) br1

let inter br1 br2 =
  let l = Array.length br1 in
  let res = Array.init l
    (fun i -> Array.init l
      (fun j -> br1.(i).(j) ^* br2.(i).(j)))
  in cf res; res

(* Reset a clock x to a value v in m
   Preserve canonical form *)
let reset m x v =
  let m = copy m in
  let n = dims m in
  for i = 0 to n do
    m.(x).(i) <- m.(0).(i) +* (Finite v, LEQ);
    m.(i).(x) <- m.(i).(0) +* (Finite (- v), LEQ)
  done;
  m

let up m (d, r) =
  let m = copy m in
  let n = dims m in
  for i = 1 to n do
    m.(i).(0) <- move_sup_bndrel m.(i).(0) (d, r);
  done;
  m

let reachable m z =
  not (is_empty (inter (up m d_infty) z))

let enabled m z =
  not (is_empty (inter m z))

let foldi f x0 i0 n =
  let rec fo x i =
    if i > n then x
    else fo (f x i) (i+1)
  in fo x0 i0

(* PRE: g is reachable from zi *)
let in_dist zi g =
  assert (reachable zi g);
  let n = dims zi in
  let d = foldi
    (fun x i -> max_bndrel x (dist_bndrel zi.(i).(0) (abs_bndrel g.(0).(i))))
    d_zero 1 n
  in d

(* PRE: g is reachable from zi *)
let out_dist zi g =
  assert (reachable zi g);
  let n = dims zi in
  let d = foldi
    (fun x i -> min_bndrel x (dist_bndrel (abs_bndrel zi.(0).(i)) g.(i).(0)))
    d_infty 1 n
  in min_bndrel d d_infty

let dist zi g  =
  if reachable zi g then
    if enabled zi g then [out_dist zi g]
    else [(in_dist zi g); (out_dist zi g)]
  else []


let move m d1 d2 =
  let m = copy m in
  let n = dims m in
  for i = 1 to n do
    if d2 <> d_zero then
      m.(i).(0) <- move_sup_bndrel m.(i).(0) d2;
    if d1 <> d_infty then
      m.(0).(i) <- move_inf_bndrel m.(0).(i) (compl_bndrel d1);
  done;
  m

let contour d1 d2 max1 max2 m =
  let b1, r1 = box_bndrel m.(0).(d1)
  and b2, r2 = box_bndrel m.(d1).(d2)
  and b3, r3 = box_bndrel m.(d1).(0)
  and b4, r4 = box_bndrel m.(d2).(0)
  and b5, r5 = box_bndrel m.(d2).(d1)
  and b6, r6 = box_bndrel m.(0).(d2) in

  let x1, y1 as p1 = saturate_bnd max1 b1, saturate_bnd max2 b6 in
  let x4, y4 as p4 = saturate_bnd max1 b3, saturate_bnd max2 b4 in

  let p2, p3 =
    match b2 with
    | Finite x -> (y1 + x, y1), (x4, x4 - x)
    | Infinite -> (x4, y1), (x4, y1)
  in
  let p5, p6 =
    match b5 with
    | Finite x -> (y4 - x, y4), (x1, x1 + x)
    | Infinite -> (x1, y4), (x1, y4)
  in

  [|p1;p2;p3;p4;p5;p6;p1|],
  [|r6;r2;r3;r4;r5;r1|]
