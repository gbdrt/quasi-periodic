type rel = | LEQ | LT
type bnd = | Finite of int | Infinite
type dist = (bnd * rel)
type dbm = (bnd * rel) array array
type guard =  {gident: int; gcond: dbm}

type clkexp =
| Clock     of int
| ClockDiff of int * int

type cond =
| Upper of clkexp * rel * int
| Lower of int * rel * clkexp
| Both  of int * rel * clkexp * rel * int
