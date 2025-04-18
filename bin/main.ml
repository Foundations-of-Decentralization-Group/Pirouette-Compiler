let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json>]"
let ast_dump_format = ref "pprint"
let msg_backend = ref ""
let file_ic = ref None
let basename = ref ""

let anon_fun filename =
  basename := Filename.remove_extension (Filename.basename filename);
  file_ic := Some (open_in filename)
;;

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-ast-dump"
    , Arg.Symbol ([ "pprint"; "json" ], fun s -> ast_dump_format := s)
    , "Dump the AST in the specified format (pprint, json)" )
  ; ( "-msg-backend"
    , Arg.Symbol ([ "domain"; "mpi" ], fun s -> msg_backend := s)
    , "Specify the backend for parallel execution (domain, mpi)" )
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.Parse.parse_with_error lexbuf in
  (match !ast_dump_format with
   | "json" -> Ast_utils.jsonify_choreo_ast (open_out (!basename ^ ".json")) program
   | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (!basename ^ ".ast")) program
   | _ -> invalid_arg "Invalid ast-dump format");
  let locs = Ast_utils.extract_locs program in
  let net_stmtblocks = List.map (fun loc -> Netgen.epp_choreo_to_net program loc) locs in
  List.iter2
    (fun loc stmtblock ->
       match !ast_dump_format with
       | "json" ->
         Ast_utils.jsonify_net_ast (open_out (!basename ^ "." ^ loc ^ ".json")) stmtblock
       | "pprint" ->
         Ast_utils.pprint_net_ast (open_out (!basename ^ "." ^ loc ^ ".ast")) stmtblock
       | _ -> invalid_arg "Invalid ast-dump format")
    locs
    net_stmtblocks;
  match !msg_backend with
  | "domain" ->
    Ocamlgen.Toplevel_domain.emit_toplevel_domain
      (open_out (!basename ^ ".ml"))
      locs
      net_stmtblocks
  | "mpi" ->
    Ocamlgen.Toplevel_mpi.emit_toplevel_mpi
      (open_out (!basename ^ ".ml"))
      locs
      net_stmtblocks
  | _ -> invalid_arg "Invalid backend"
;;
