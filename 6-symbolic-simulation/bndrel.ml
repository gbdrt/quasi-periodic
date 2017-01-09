open Format
open Symb

(** Relations **)

let lt_rel r1 r2 = match r1, r2 with
  | LT, LEQ -> true
  | _ -> false

let min_rel r1 r2 = match r1, r2 with
  | LEQ, LEQ -> LEQ
  | _ -> LT

let op_rel r = match r with
  | LEQ -> LT
  | LT -> LEQ

let pr_rel ppf = function
  | LT  -> fprintf ppf "<"
  | LEQ -> fprintf ppf "<="

(** Bounds **)

let op_bnd r = match r with
  | Finite x -> Finite (-x)
  | Infinite -> Finite 0

let min_bnd b1 b2 =
  match b1, b2 with
  | Finite x, Finite y -> Finite (min x y)
  | Finite x, Infinite -> Finite x
  | Infinite, _ -> b2

let lt_bnd r1 r2 = match r1, r2 with
  | Finite x, Finite y -> x < y
  | Finite _, Infinite -> true
  | _ -> false

let eq_bnd r1 r2 = match r1, r2 with
  | Infinite, Infinite -> true
  | Finite x, Finite y -> x = y
  | _ -> false

let plus_bnd b1 b2 = match b1, b2 with
  | Finite x, Finite y -> Finite (x + y)
  | _ -> Infinite

let saturate_bnd mx b =
  match b with
  | Finite x when 0 <= x -> x
  | Finite x when x < 0 -> -x
  | Infinite -> mx
  | _ -> assert false

let pr_bnd ppf = function
  | Infinite    -> fprintf ppf "∞"
  | Finite i    -> fprintf ppf "%d" i


(** (Bound, Relation) **)

let b_zero = (Infinite, LEQ)   (* n: zero element (∞, <) *)
let b_one  = (Finite 0, LEQ)  (* e: unit element (0, ≤) *)

let pr_bndrel ppf (b, r) =
  fprintf ppf "(%a, %a)" pr_bnd b pr_rel r

let pr_bndrel_debug ppf (b, r) =
  let pr_bnd ppf = function
    | Infinite    -> fprintf ppf " ∞"
    | Finite i    -> fprintf ppf "%2d" i
  in
  let pr_rel ppf = function
    | LT  -> fprintf ppf "< "
    | LEQ -> fprintf ppf "<="
  in
  fprintf ppf "(%a,%a)" pr_bnd b pr_rel r

let lt_bndrel (b1, r1) (b2, r2) =
  lt_bnd b1 b2 || (b1 = b2 && lt_rel r1 r2)

let min_bndrel br1 br2 =
  if lt_bndrel br1 br2 then br1 else br2

let max_bndrel br1 br2 =
  if lt_bndrel br1 br2 then br2 else br1

let compl_bndrel (b, r) = b, op_rel r

let abs_bndrel (b, r) = match b with
  | Finite x -> Finite (abs x), r
  | Infinite -> Infinite, r

let minus_bndrel (b1, r1) (b2, r2) =
  match b1, b2 with
  | Finite x, Finite y ->
    if x = y then Finite 0, LT else Finite (x - y), r1
  | _ -> b_zero

let box_bndrel (b,r) = match b with
  | Finite _ -> b, r
  | Infinite -> b, LEQ


(* Regular algebra, see Dill, 1989 *)

(* multiplication: + *)
let (+* ) (b1, r1) (b2, r2) =
  match b1, b2 with
  | Finite i1, Finite i2 -> (Finite (i1 + i2), min_rel r1 r2)
  | _ -> (Infinite, min_rel r1 r2)

(* addition: ⊓ *)
let (^* ) br1  br2 = min_bndrel br1 br2

(** Distance **)

let d_zero = Finite 0, LT
let d_infty = Infinite, LT

let dist_bndrel br1 br2 =
  if lt_bndrel br1 br2 then compl_bndrel (minus_bndrel br2 br1)
  else d_zero

let move_sup_bndrel (b, r) (d, rd) =
  match b, d with
  | Finite x, Finite y -> Finite (x + y), rd
  | _ -> d_infty

let move_inf_bndrel (b,r) (d, rd) =
  match b, d with
  | Finite x, Finite y -> Finite (x - y), rd
  | _ -> d_infty


let mindist dl dp  =
  let ddl =  List.filter (fun x -> lt_bndrel dp x) dl in
  let dn = List.fold_left (min_bndrel) d_infty ddl in
  dn
