(* Random generator *)
let arbitrary min max =
  (Random.float (max -. min)) +. min

(* Queue API *)
let empty = []
let front l = List.hd (List.rev l)
let back l = List.hd l
let dequeue l = List.rev (List.tl (List.rev l))
let enqueue l i = i::l
let sum l = List.fold_left (+.) 0.0 l
let size l = List.length l
let is_empty l = (l == [])

(* Option type *)
type 'a opt = One of 'a | Zero
let some x = One(x)
let none = Zero
let get x = match x with
  | Zero -> failwith "Error cannot get value from None"
  | One(v) -> v


(* Print *)
let print1 o1 =
  Format.printf "o1 = %d@." o1

let print2 o2 =
  Format.printf "\t\to2 = %d@." o2

let dummy a b = ()
