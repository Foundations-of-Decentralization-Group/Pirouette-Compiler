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

let rec dump_network_type = function
  | Unit _ -> "unit"
  | TMap (nt1, nt2) -> "map (" ^ dump_network_type nt1 ^ ", " ^ dump_network_type nt2 ^ ")"
  | TProd (nt1, nt2) -> "product (" ^ dump_network_type nt1 ^ ", " ^ dump_network_type nt2 ^ ")"
  | TSum (nt1, nt2) -> "sum (" ^ dump_network_type nt1 ^ ", " ^ dump_network_type nt2 ^ ")"

let dump_atom = function
  | Int i ->
      "Int " ^ string_of_int i
  | Float f ->
      "Float " ^ string_of_float f
  | Word s ->
      "Word \"" ^ s ^ "\""

let dump_program (Program atoms) =
  "[" ^ (String.concat ", " (List.map dump_atom atoms)) ^ "]"