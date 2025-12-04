(* Parse the standard library file pointed to by the path specified in PIR_STDLIB environment variable on the user's system *)
let get_stdlib_ast ?(recompile=false) (locs : string list) : 'a Ast_core.Choreo.M.stmt_block  = 

    if recompile then Stdlib_compiler.compile_stdlib ();
    let rec rename_locs ast = function
    | [] -> []
    | h::d -> Stdlib_loc_linker.ast_list_domain_rename h ast @ rename_locs ast d

    in
    rename_locs Stdlib_ast.ast locs
;;