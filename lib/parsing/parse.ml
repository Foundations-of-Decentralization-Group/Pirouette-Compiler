open Lexing

let spf = Printf.sprintf

let string_of_pos pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (pos.pos_cnum - pos.pos_bol) in
  spf "[Ln %s, Col %s]" l c
;;

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    failwith (spf "Syntax error at %s: %s" (string_of_pos lexbuf.lex_start_p) msg)
  | Parser.Error -> failwith (spf "Parse error at %s" (string_of_pos lexbuf.lex_start_p))
;;
