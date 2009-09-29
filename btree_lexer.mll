(* this stuff gets evaluated automatically *)
{
  open Btree_parser
  let line = ref 1
}

let comma = ','
let openp = '('
let closep = ')'
let digit = ['0'-'9']
let integer = digit+

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | '\n'            { incr line; CR }
  (* because taxnames can be floats, we have to have float first *)
  | integer         { INTEGER(int_of_string (Lexing.lexeme lexbuf)) }
  | comma           { COMMA }
  | openp           { OPENP }
  | closep          { CLOSEP }
  | eof             { EOF }
  | _               { failwith ("Problem reading tree at line "^(string_of_int !line)) }


