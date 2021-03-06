val arbitrary : float -> float -> float

val empty : 'a list
val front : 'a list -> 'a
val back : 'a list -> 'a
val dequeue : 'a list -> 'a list
val enqueue : 'a list -> 'a -> 'a list
val sum : float list -> float
val is_empty : 'a list -> bool
val size : 'a list -> int

type 'a opt
val some: 'a -> 'a opt
val none: 'a opt
val get: 'a opt -> 'a

val print1: int -> unit
val print2: int -> unit
val dummy: 'a -> 'b -> unit
