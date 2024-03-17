val parse_program : Lexing.lexbuf -> Ast.Choreo.program
(** [parse_program lexbuf] parses the input from [lexbuf] into an AST program.
    Raises [Failure] with an error message if parsing fails. *)
