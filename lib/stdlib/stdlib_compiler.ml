  (* Check if there exists a PIR_STDLIB environment variable on the user's system. If not, print on stderr and exit *)
let compile_stdlib () : unit =
  let (path_to_stdlib:string) = match Sys.getenv_opt "PIR_STDLIB" with
    | Some p ->
        p
    | None ->
        prerr_endline ("Path to Pirouette standard library not set in enviroment variables as \"PIR_STDLIB\"=[ABSOLUTE_PATH_TO_YOUR_STDLIB]\n"); exit 1
  in  

  (* Create an OCaml file representation -via opening the file for reading as an in channel- of the path to the standard library file *)
  let file_ic_stlid = open_in path_to_stdlib in
  
  (* Lex the file *)
  let lexbuf_stdlib = Lexing.from_channel file_ic_stlid in
    (* Return the AST created from the parsed lex *)
  let ast_stdlib = Parsing.Parse.parse_with_error (path_to_stdlib) (lexbuf_stdlib) in 

  let stdlib_ast_str = Marshal.to_string ast_stdlib [] in
  (* There must be a stdlib_ast.ml file in the same directory as your stdlib.pir pointed to by your PIR_STDLIB env var*)
  let stdlib_ast_file_oc = open_out ((Filename.dirname path_to_stdlib)^Filename.dir_sep^"stdlib_ast.ml") in

  (* We save the Marshalled AST of the stdlib to stdlib_ast.ml, but wrapped within OCaml code that allows us to reference that value *)
  output_string stdlib_ast_file_oc ("let ast={|"^ stdlib_ast_str ^ "|};;"); close_out stdlib_ast_file_oc;
;;