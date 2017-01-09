open Symb
open Bndrel

let nb_clk = 2

let kill x = exit 0

let empty_list () = []
let cons x l = x :: l
let concat l1 l2 = l1 @ l2
let nth = List.nth

let rec filter l1 l2 =
  match l1, l2 with
  | h1::t1, h2::t2 -> if h2 then h1::(filter t1 t2) else (filter t1 t2)
  | [], [] -> []
  | _ -> []


type dist = bnd * rel
let dzero = Bndrel.d_zero
let mindist = Bndrel.mindist


type zone = Symb.dbm
let zmake = Dbm.from_string nb_clk
let zall = zmake ""
let is_zempty = Dbm.is_empty
let zequal = Dbm.equal
let zreset = Dbm.reset
let zinter = Dbm.inter
let zinterfold = List.fold_left zinter zall
let zup m = Dbm.up m b_zero
let zdist = Dbm.dist
let zdistmap zi gl = List.fold_left(fun a zg -> a @ zdist zi zg) [] gl
let zsweep = Dbm.move
let zenabled zc gl = List.map (fun zg -> Dbm.enabled zc zg) gl

let zprint s z = Format.printf "%s: %a@\n" s Dbm.dump z

let zdraw d1 d2 max1 max2 m =
  let contour = Dbm.contour d1 d2 max1 max2 in
  let ct = contour m in
  zprint "current zone" m;
  Draw.plot 100 50 max1 max2 ct

let print_guards bw (sg1, bg1) (sg2, bg2) =
  if bw then Format.printf "- wait@\n";
  if bg1 then Format.printf "- %s@\n" sg1;
  if bg2 then Format.printf "- %s@\n" sg2;
  0
