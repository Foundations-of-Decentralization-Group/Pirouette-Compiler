(* Parse the standard library file pointed to by the path specified in PIR_STDLIB environment variable on the user's system *)
let get_stdlib_ast ?(recompile=false) () : unit = 

    if recompile then (Stdlib_compiler.compile_stdlib ()) else ();
;;