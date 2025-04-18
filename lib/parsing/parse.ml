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

let parse_net_with_error lexbuf =
  try Net_parser.prog Net_lexer.read lexbuf with
  | Net_lexer.SyntaxError msg ->
    failwith (spf "Syntax error at %s: %s" (string_of_pos lexbuf.lex_start_p) msg)
  | Net_parser.Error -> 
    let token = Lexing.lexeme lexbuf in
    let pos = string_of_pos lexbuf.lex_start_p in
    let pos_cnum = lexbuf.lex_start_p.pos_cnum in
    let input_str = lexbuf.lex_buffer in
    
    (* Get a context window around the error position *)
    let context_start = max 0 (pos_cnum - 20) in
    let context_end = min (Bytes.length input_str) (pos_cnum + 20) in
    let context = Bytes.sub_string input_str context_start (context_end - context_start) in
    
    (* Add markers to show exactly where the error is *)
    let error_marker = String.make (pos_cnum - context_start) ' ' ^ "^" in
    
    failwith (spf "Parse error at %s with token '%s'.\nContext: \"%s\"\nPosition: %s\nCurrent offset: %d\nBuffer start/end: %d/%d"
                 pos token context error_marker 
                 lexbuf.lex_curr_pos lexbuf.lex_start_pos lexbuf.lex_buffer_len)
;;
