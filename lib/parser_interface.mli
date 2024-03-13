val parse_program : Lexing.lexbuf -> Choreo_ast.program
(** [parse_program lexbuf] parses the input from [lexbuf] into an AST program.
    Raises [Failure] with an error message if parsing fails. *)

val dump_ast : Choreo_ast.program -> string
(** [dump_ast prog] converts the AST [prog] into a JSON string. *)
