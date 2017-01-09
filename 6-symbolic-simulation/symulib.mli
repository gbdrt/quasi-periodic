type dist
type zone

val kill: unit -> 'a
val empty_list : unit -> 'a list
val cons : 'a -> 'a list -> 'a list
val concat : 'a list -> 'a list -> 'a list
val nth : 'a list -> int -> 'a
val filter: 'a list -> bool list -> 'a list

val dzero : dist
val mindist: dist list -> dist -> dist

val zall : zone
val zmake : string -> zone
val is_zempty : zone -> bool
val zequal : zone -> zone -> bool
val zreset : zone -> int -> int -> zone
val zinter : zone -> zone -> zone
val zinterfold : zone list -> zone
val zup : zone -> zone
val zdist: zone -> zone -> dist list
val zdistmap: zone -> zone list -> dist list
val zsweep : zone -> dist -> dist -> zone
val zenabled: zone -> zone list -> bool list

val zprint : string -> zone -> unit
val zdraw : int -> int -> int -> int -> zone -> unit

val print_guards : bool -> (string * bool) -> (string * bool) -> int
