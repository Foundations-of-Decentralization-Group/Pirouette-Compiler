(* Parse the standard library file pointed to by the path specified in PIR_STDLIB environment variable on the user's system *)
let parse_stdlib () : Parsing.Parsed_ast.Choreo.stmt_block = 

  (* Check if there exists a PIR_STDLIB environment variable on the user's system. If not, print on stderr and exit *)
  let (path_to_stdlib:string) = match Sys.getenv_opt "PIR_STDLIB" with
    | Some p ->
        p
    | None ->
        prerr_endline ("Path to Pirouette standard library not set in enviroment variables as \"PIR_STDLIB\"=[ABSOLUTE_PATH_TO_YOUR_STDLIB]\n"); exit 1
  in

  (* Create an OCaml file representation -via opening the file for reading as an in channel- of the path to the standard library file *)
  let file_ic_stlid = Some (open_in path_to_stdlib) in
    (* If unable to open the file for reading, print on stderr and exit *)
    (if file_ic_stlid = None
    then (
      prerr_endline ("No stdlib");
      exit 1)); 
  
      (* Lex the file *)
    let lexbuf_stdlib = Lexing.from_channel (Option.get file_ic_stlid) in
      (* Return the AST created from the parsed lex *)
      (Parsing.Parse.parse_with_error (path_to_stdlib) (lexbuf_stdlib))
;;