open Lexer
open Lexing

module Pos_info = struct
  type t =
    { fname : string
    ; start : int * int (* line, column *)
    ; stop : int * int (* line, column *)
    }
end

module Choreo = Ast_core.Choreo.With (Pos_info)

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  "line " ^ l ^ ", column " ^ c
;;

let parse_program lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
    raise (Failure ("Syntax error: " ^ msg ^ " at " ^ pos_string lexbuf.lex_curr_p))
  | Parser.Error -> raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))
;;
