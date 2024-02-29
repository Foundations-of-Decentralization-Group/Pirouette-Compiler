{
  open Lexing
  open Parser

  let advance_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let integer = sign? digit+
let identifier = alpha (alpha | digit | '_')*

let whitespace = [' ' '\t']+

(* Rules *)

rule token = parse
  | integer as v { INT_CONSTANT (int_of_string v) }
  | float_constant { FLOAT_CONSTANT (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { WORD (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
