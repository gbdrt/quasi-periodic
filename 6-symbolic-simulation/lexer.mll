{
  open Parser
  open Lexing

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

rule token = parse
  | '\n'              { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | '<'               { LESS }
  | "<="              { LESS_EQ }
  | '>'               { GREATER }
  | ">="              { GREATER_EQ }
  | '='               { EQUAL }
  | '-'               { MINUS }
  | "&&"              { AND_AND }
  | ','               { COMMA }

  | '-'?['0'-'9']+        { INTEGER (int_of_string (lexeme lexbuf)) }
  | 'x' (['0'-'9']+ as n) { CLOCK (int_of_string n) }

  | eof               { EOF }
  | _                 { assert false }
