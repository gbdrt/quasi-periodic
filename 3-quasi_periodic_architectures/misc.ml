(* Random generator *)
let arbitrary min max = (Random.float (max -. min)) +. min

(* Queue API *)
let empty = []
let is_empty l = (l == [])
let front l = List.hd (List.rev l)
let back l = List.hd l
let dequeue l = List.rev (List.tl (List.rev l))
let enqueue l i = i::l
let size l = List.length l

(* Print *)
let print o =
  Format.printf "cmd = %d\n" o;
  Format.print_flush ()

(* Option type *)
type 'a opt = One of 'a | Zero
let some x = One(x)
let none = Zero
let get x = match x with
  | Zero -> failwith "Error cannot get value from None"
  | One(v) -> v
