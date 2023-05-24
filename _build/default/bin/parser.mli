
(* The type of tokens. *)

type token = 
  | RParen
  | Lambda
  | LParen
  | Identifier of (string)
  | EOF
  | END
  | Dot

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr option)
