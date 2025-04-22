let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json|dot>] [-backend <shm|http>]"
let ast_dump_format = ref "pprint"
let backend = ref "shm"
let file_ic = ref None
let input_filename = ref "" (* Store the original input filename *)
let basename = ref "" (* Store just the base part, no directory/extension *)

let anon_fun filename =
  input_filename := filename; (* Keep the original path *)
  basename := Filename.basename (Filename.remove_extension filename); (* Extract just the base name *)
  file_ic := Some (open_in filename)
;;

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-ast-dump"
    , Arg.Symbol ([ "pprint"; "json"; "dot" ], fun s -> ast_dump_format := s)
    , "Dump the AST in the specified format (pprint, json, dot)" )
  ; ( "-backend"
    , Arg.Symbol ([ "shm"; "http" ], fun s -> backend := s)
    , "Choose communication backend (shm: shared memory, http: HTTP)" )
  ]
;;

(* Helper function to generate the dune file *)
let generate_dune_file base_name locs backend =
  let common_libs = "ast_core parsing codegen ast_utils netgen ppxlib" in
  let dune_path, dune_content =
    match backend with
    | "shm" ->
      let content = Printf.sprintf
        {|(executable
 (name %s)
 (libraries %s domainslib))
|}
        base_name (* Use the extracted base name *)
        common_libs
      in
      ("dune", content) (* Output dune file to the root for shm *)

    | "http" ->
      let executable_names =
        List.map (fun loc -> Printf.sprintf "%s_%s" base_name loc) locs (* Use the extracted base name *)
        |> String.concat " "
      in
      let content = Printf.sprintf
        {|(executables
 (names %s)
 (libraries %s http_pirc lwt cohttp-lwt-unix yojson))
|}
        executable_names
        common_libs
      in
      (Filename.concat "examples" "dune", content) (* Output dune file to examples/ for http *)

    | _ -> invalid_arg "Invalid backend for dune generation"
  in
  let oc = open_out dune_path in
  output_string oc dune_content;
  close_out oc
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !file_ic = None || !basename = "" (* Check if basename was set *)
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);

  (* Determine output directory for AST dumps based on input file path *)
  let output_dir = Filename.dirname !input_filename in
  let get_output_path suffix = Filename.concat output_dir (!basename ^ suffix) in

  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.Parse.parse_with_error lexbuf in
  (match !ast_dump_format with
   | "json" -> Ast_utils.jsonify_choreo_ast (open_out (get_output_path ".json")) program
   | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (get_output_path ".ast")) program
   | "dot" ->
      let string_of_info = Parsing.Parsed_ast.Pos_info.string_of_pos in
      Ast_utils.dot_choreo_ast (open_out (get_output_path ".dot")) string_of_info program
   | _ -> invalid_arg "Invalid ast-dump format");

  let locs = Ast_utils.extract_locs program in
  let netir_l = List.map (fun loc -> Netgen.epp_choreo_to_net program loc) locs in
  List.iter2
    (fun loc ir ->
      match !ast_dump_format with
      | "json" ->
        Ast_utils.jsonify_net_ast (open_out (get_output_path ("." ^ loc ^ ".json"))) ir
      | "pprint" ->
        Ast_utils.pprint_net_ast (open_out (get_output_path ("." ^ loc ^ ".ast"))) ir
      | "dot" -> () (* Skip dot generation for network IR *)
      | _ -> invalid_arg "Invalid ast-dump format")
    locs
    netir_l;

  (* Generate OCaml code based on backend *)
  (match !backend with
  | "shm" ->
    (* SHM backend: generates a single .ml file in the root *)
    let msg_module = (module Codegen.Msg_intf.Msg_chan_intf : Codegen.Msg_intf.M) in
    let out_path = !basename ^ ".ml" in (* Output to root: e.g., ex1.ml *)
    Codegen.Toplevel_shm.emit_toplevel_shm (open_out out_path) msg_module locs netir_l

  | "http" ->
    (* HTTP backend: generates multiple .ml files, one per location, inside the examples/ directory *)
    let msg_module = (module Codegen.Msg_intf.Msg_http_intf : Codegen.Msg_intf.M) in
    let examples_dir = "examples" in
    (* Ensure the examples directory exists *)
    if not (Sys.file_exists examples_dir && Sys.is_directory examples_dir) then
      Unix.mkdir examples_dir 0o755;

    List.iter2
      (fun loc ir ->
        (* Use the extracted base name here *)
        let ml_filename = Printf.sprintf "%s_%s.ml" !basename loc in
        let out_path = Filename.concat examples_dir ml_filename in (* Output to examples/ *)
        let out_file = open_out out_path in
        output_string out_file "open Http_pirc\n\n";
        Codegen.Toplevel_shm.emit_toplevel_http out_file msg_module [ loc ] [ ir ];
        close_out out_file)
      locs
      netir_l
  | _ -> invalid_arg "Invalid backend");

  (* Generate the dune file after generating all ml files *)
  generate_dune_file !basename locs !backend
;;
