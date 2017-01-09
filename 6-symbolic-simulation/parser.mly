%{
  open Symb
%}

%token MINUS
%token <int> INTEGER
%token <int> CLOCK
%token LESS LESS_EQ GREATER GREATER_EQ EQUAL
%token AND_AND COMMA
%token EOF

%left AND_AND COMMA


%type <Symb.cond list> relation_list
%start relation_list

%%

relation_list:
    /* empty */
    { [] }
  | relation_list relation
    { $2 :: $1 }
  | relation_list AND_AND relation
    { $3 :: $1 }
  | relation_list COMMA relation
    { $3 :: $1 }

relation:
  | INTEGER  lessrel clockexp
    { Lower ($1, $2, $3) }
  | CLOCK lessrel INTEGER
    { Upper (Clock $1, $2, $3) }
  | CLOCK MINUS CLOCK lessrel INTEGER
    { Upper (ClockDiff ($1, $3), $4, $5) }
  | CLOCK EQUAL INTEGER
    { Both ($3, LEQ, Clock $1, LEQ, $3) }
  | CLOCK MINUS CLOCK EQUAL INTEGER
    { Both ($5, LEQ, ClockDiff ($1, $3), LEQ, $5) }
  | INTEGER  EQUAL clockexp
    { Both ($1, LEQ, $3, LEQ, $1) }
  | CLOCK EQUAL CLOCK
    { Both (0, LEQ, ClockDiff ($1, $3), LEQ, 0) }
  | CLOCK lessrel CLOCK
    { Upper (ClockDiff ($1, $3), $2, 0) }
  | INTEGER lessrel clockexp lessrel INTEGER
    { Both ($1, $2, $3, $4, $5) }

clockexp:
  | CLOCK MINUS CLOCK
    { ClockDiff ($1, $3) }
  | CLOCK
    { Clock $1 }

lessrel:
  | LESS
    { LT }
  | LESS_EQ
    { LEQ }
