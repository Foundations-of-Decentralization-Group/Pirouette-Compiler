(* Parse the standard library file pointed to by the path specified in PIR_STDLIB environment variable on the user's system *)
let get_stdlib_ast ?(recompile=false) () : Parsing.Parsed_ast.Choreo.stmt_block = 

    if recompile then (Stdlib_compiler.compile_stdlib (); Marshal.from_string Stdlib_ast.ast 0) else (Marshal.from_string Stdlib_ast.ast 0);
;;