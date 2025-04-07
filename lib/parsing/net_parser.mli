
(* The type of tokens. *)

type token = 
  | WITH
  | UNIT_T
  | UNDERSCORE
  | TYPE
  | TRUE
  | TIMES
  | TILDE_ARROW
  | THEN
  | STRING_T
  | STRING of (string)
  | SND
  | SEND
  | SEMICOLON
  | RPAREN
  | RIGHT
  | RET
  | RECV
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS
  | MATCH
  | LT
  | LPAREN
  | LET
  | LEQ
  | LEFT
  | INT_T
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | GT
  | GEQ
  | FUN
  | FST
  | FROM
  | FOREIGN
  | FOR
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | COLONEQ
  | COLON
  | CHOOSE
  | CHOICE
  | BOOL_T
  | BAR
  | ARROW
  | AND
  | ALLOW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parsed_ast.Net.stmt_block)
