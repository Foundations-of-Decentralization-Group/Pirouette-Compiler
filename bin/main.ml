let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json>]"
let ast_dump_format = ref "pprint"
let file_ic = ref None
let basename = ref "choreo"

let anon_fun filename =
  basename := Filename.remove_extension filename;
  file_ic := Some (open_in filename)
;;

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-ast-dump"
    , Arg.Symbol ([ "pprint"; "json" ], fun s -> ast_dump_format := s)
    , "Dump the AST in the specified format (pprint, json)" )
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.parse_program lexbuf in
  (match !ast_dump_format with
   | "json" -> Ast_utils.jsonify_choreo_ast (open_out (!basename ^ ".json")) program
   | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (!basename ^ ".ast")) program
   | _ -> invalid_arg "Invalid ast-dump format");
  let locs = Ast_utils.extract_locs program in
  List.iter
    (fun loc ->
      let ir = Irgen.epp program loc in
      match !ast_dump_format with
      | "json" ->
        Ast_utils.jsonify_net_ast (open_out (!basename ^ "." ^ loc ^ ".json")) ir
      | "pprint" ->
        Ast_utils.pprint_net_ast (open_out (!basename ^ "." ^ loc ^ ".ast")) ir
      | _ -> invalid_arg "Invalid ast-dump format")
    locs
;;
