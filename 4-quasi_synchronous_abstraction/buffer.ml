type buffer =
  {
    content : int array;
    mutable pos : int;
    size : int;
  }

let buffer n =
  {
    content = Array.make n 0;
    pos = 0;
    size = n
  }

let push v b =
  b.content.(b.pos mod b.size) <- v;
  b.pos <- b.pos + 1

let sum b = Array.fold_left (fun x y -> x + y) 0 b.content
