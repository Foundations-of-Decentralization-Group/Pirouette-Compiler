open Ast
open Lexing

let colnum pos =
  (pos.pos_cnum - pos.pos_bol) - 1

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int ((colnum pos) + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f s =
  let lexbuf = Lexing.from_string s in
  try
    f Lexer.token lexbuf
  with Parser.Error ->
    raise (Failure ("Parse error at " ^ (pos_string lexbuf.lex_curr_p)))

let parse_program s =
  parse' Parser.program s

let dump_program (Prog decl_block) =
  let rec dump_decl_block = function
    | [] -> ""
    | (Decl (id, t)) :: rest ->
      "Decl(" ^ id ^ ", " ^ (dump_type t) ^ ");\n" ^ (dump_decl_block rest)
  and dump_type = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Fun (t1, t2) -> "Fun(" ^ (dump_type t1) ^ ", " ^ (dump_type t2) ^ ")"
  in
  "Program {\n" ^ (dump_decl_block decl_block) ^ "}\n"