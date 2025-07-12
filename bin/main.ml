(* Pirouette Compiler (pirc) main entry point *)

(* Command line configuration *)
let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json|dot>] [-msg-backend <domain|http|mpi>]"
let ast_dump_format = ref "pprint"
let msg_backend = ref "domain"
let file_ic = ref None
let input_filename = ref "" (* Original input filename with path *)

(* Process anonymous command line argument (input file) *)
let process_input_file filename =
  input_filename := filename;
  file_ic := Some (open_in filename)
;;

(* Command line options specification *)
let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-ast-dump"
    , Arg.Symbol ([ "pprint"; "json"; "dot" ], fun s -> ast_dump_format := s)
    , "Dump the AST in the specified format (pprint, json, dot)" )
  ; ( "-msg-backend"
    , Arg.Symbol ([ "domain"; "http"; "mpi" ], fun s -> msg_backend := s)
    , "Choose communication backend (domain: shared memory, http: HTTP, mpi: MPI)" )
  ]
;;

(* Change the extension of a filename *)
let change_extension filename new_suffix = (Filename.remove_extension filename) ^ new_suffix ;;

(* Dump AST in specified format *)
let dump_choreo_ast format output_path program =
  match format with
  | "json" -> Ast_utils.jsonify_choreo_ast (open_out output_path) program
  | "pprint" -> Ast_utils.pprint_choreo_ast (open_out output_path) program
  | "dot" ->
    let string_of_info = Parsing.Parsed_ast.Pos_info.string_of_pos in
    Ast_utils.dot_choreo_ast (open_out output_path) string_of_info program
  | _ -> invalid_arg "Invalid ast-dump format"
;;

(* Dump network AST in specified format *)
let dump_net_ast format output_path ir =
  match format with
  | "json" -> Ast_utils.jsonify_net_ast (open_out output_path) ir
  | "pprint" -> Ast_utils.pprint_net_ast (open_out output_path) ir
  | "dot" -> () (* Skip dot generation for network IR *)
  | _ -> invalid_arg "Invalid ast-dump format"
;;

(* Generate code for domains backend *)
let generate_domain_code filename locs netir_l =
  let out_path = change_extension filename ".domain.ml" in
  Ocamlgen.Toplevel_domain.emit_toplevel_domain (open_out out_path) locs netir_l
;;

(* Generate code for HTTP backend *)
let generate_http_code filename locs netir_l =
  (* Generate one file per location *)
  List.iter2
    (fun loc ir ->
       let out_path = change_extension filename ("_" ^ loc ^ ".ml") in
       let out_file = open_out out_path in
       (* Add appropriate imports *)
       output_string out_file "open Http_pirc\n\n";
       let config_file_path = change_extension filename ".yaml" in
       Ocamlgen.Toplevel_http.emit_toplevel_http out_file [ loc ] [ ir ] config_file_path;
       close_out out_file)
    locs
    netir_l
;;

(* Generate code for MPI backend *)
let generate_mpi_code filename locs netir_l =
  let out_path = change_extension filename ".mpi.ml" in
  Ocamlgen.Toplevel_mpi.emit_toplevel_mpi (open_out out_path) locs netir_l
;;

(* Main entry point *)
let () =
  (* Parse command line arguments *)
  Arg.parse speclist process_input_file usage_msg;
  (* Check if input file was provided *)
  if !file_ic = None || !input_filename = ""
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);
  (* Parse the input file *)
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.Parse.parse_with_error lexbuf in
  (* Dump the choreography AST *)
  dump_choreo_ast
    !ast_dump_format
    (change_extension
      !input_filename
       (match !ast_dump_format with
        | "json" -> ".json"
        | "pprint" -> ".ast"
        | "dot" -> ".dot"
        | _ -> invalid_arg "Invalid ast-dump format"))
    program;
  (* Extract locations and generate network IR *)
  let locs = Ast_utils.extract_locs program in
  let netir_l = List.map (fun loc -> Netgen.epp_choreo_to_net program loc) locs in
  (* Dump network ASTs *)
  List.iter2
    (fun loc ir ->
       dump_net_ast
         !ast_dump_format
         (change_extension
           !input_filename
            ("."
             ^ loc
             ^
             match !ast_dump_format with
             | "json" -> ".json"
             | "pprint" -> ".ast"
             | _ -> ""))
         ir)
    locs
    netir_l;
  (* Generate code based on selected backend *)
  (match !msg_backend with
   | "domain" -> generate_domain_code !input_filename locs netir_l
   | "http" -> generate_http_code !input_filename locs netir_l
   | "mpi" -> generate_mpi_code !input_filename locs netir_l
   | _ -> invalid_arg "Invalid backend")
;;
