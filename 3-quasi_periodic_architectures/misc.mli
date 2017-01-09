val arbitrary : float -> float -> float

val empty : 'a list
val is_empty : 'a list -> bool
val front : 'a list -> 'a
val back : 'a list -> 'a
val dequeue : 'a list -> 'a list
val enqueue : 'a list -> 'a -> 'a list
val size : 'a list -> int

val print: int -> unit

type 'a opt
val some: 'a -> 'a opt
val none: 'a opt
val get: 'a opt -> 'a
