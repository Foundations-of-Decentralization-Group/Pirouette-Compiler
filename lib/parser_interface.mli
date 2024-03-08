open Lexing

val parse_program : lexbuf -> Ast.program
(** [parse_program lexbuf] parses the input from [lexbuf] into an AST program.
    Raises [Failure] with an error message if parsing fails. *)

val dump_ast : Ast.program -> string
(** [dump_ast prog] converts the AST [prog] into a JSON string. *)
