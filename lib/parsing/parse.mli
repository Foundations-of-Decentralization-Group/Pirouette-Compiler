(** Parser for the Pirouette choreographic programming language.
    
    This module provides parsing functions that convert tokenized Pirouette
    source code into Abstract Syntax Trees (ASTs). It works in conjunction
    with the lexer to transform text into structured data.
    
    {2 Parsing Pipeline}
    
    {v
      Source text  →  Lexer  →  Tokens  →  Parser  →  Parsed AST
      "x := 5"        read      [ID; COLONEQ; INT]   Choreo.stmt_block
    v}*)

    (**{2 Parse}*)

val parse_with_error : Lexing.lexbuf -> Parsed_ast.Choreo.stmt_block
(** [parse_with_error] parses choreographic Pirouette source code
    from a lexer buffer into a choreography AST.
    
    This is the main entry point for parsing Pirouette choreographies written
    by users. It produces a [Parsed_ast.Choreo.stmt_block] representing the
    global choreographic protocol.
    
    {b Parameters:}
    - [lexbuf]: lexer buffer containing the source code to parse
    
    {b Returns:} A choreographic statement block representing the parsed program.

    {b Raises:} Parsing exceptions if the source code contains syntax errors.
    Error messages include line and column information from the lexer buffer. *)

    (**{2 Parse Net}*)
    
val parse_net_with_error : Lexing.lexbuf -> Parsed_ast.Net.stmt_block
(** [parse_net_with_error] parses network IR Pirouette code from a 
    lexer buffer into a network AST.
    
    This function is typically used for parsing intermediate representations
    or for testing purposes. It produces a [Parsed_ast.Net.stmt_block] which
    is the result of endpoint projection, containing explicit send/recv
    operations rather than choreographic constructs.
    
    {b Parameters:}
    - [lexbuf]: lexer buffer containing the network IR code to parse
    
    {b Returns:} A network statement block representing the parsed program.

    {b Raises:} Parsing exceptions if the source code contains syntax errors. *)


