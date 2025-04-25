(* Pirouette Compiler (pirc) main entry point *)

(* Command line configuration *)
let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json|dot>] [-backend <shm|http>]"
let ast_dump_format = ref "pprint"
let backend = ref "shm"
let file_ic = ref None
let input_filename = ref "" (* Original input filename with path *)
let basename = ref "" (* Base name without directory/extension *)

(* Process anonymous command line argument (input file) *)
let process_input_file filename =
  input_filename := filename;
  basename := Filename.basename (Filename.remove_extension filename);
  file_ic := Some (open_in filename)
;;

(* Command line options specification *)
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

(* Extract FFI libraries from network ASTs *)
let extract_ffi_libraries netir_l =
  List.fold_left
    (fun acc ir ->
      let files = Ast_utils.collect_ffi_files ir in
      List.fold_left (fun acc' file -> file :: acc') acc files)
    []
    netir_l
  |> List.sort_uniq String.compare
  |> List.map Filename.basename
  |> List.map (fun file ->
    if Filename.check_suffix file ".ml" then Filename.remove_extension file else file)
;;

(* Generate dune file based on backend type *)
let generate_dune_file base_name locs backend netir_l =
  (* Extract FFI libraries *)
  let ffi_libs = extract_ffi_libraries netir_l in
  let common_libs = "ast_core parsing codegen ast_utils netgen ppxlib" in
  (* Add FFI libraries if they exist *)
  let library_deps =
    if List.length ffi_libs > 0
    then common_libs ^ " " ^ String.concat " " ffi_libs
    else common_libs
  in
  match backend with
  | "shm" ->
    (* For SHM backend, create a new dune file in the root *)
    let content =
      Printf.sprintf
        {|(executable
 (name %s)
 (libraries %s domainslib))
|}
        base_name
        library_deps
    in
    let oc = open_out "dune" in
    output_string oc content;
    close_out oc
  | "http" ->
    (* For HTTP backend, create a new dune file in examples/ directory *)
    let dune_path = "examples/dune" in
    (* Create executable names for each location *)
    let executable_names =
      List.map (fun loc -> Printf.sprintf "%s_%s" base_name loc) locs |> String.concat " "
    in
    (* Create library section for FFI files *)
    let lib_section =
      if List.length ffi_libs > 0
      then
        Printf.sprintf
          {|(library
 (name ffi_lib)
 (modules %s)
 (libraries %s))
|}
          (String.concat " " ffi_libs)
          common_libs
      else
        Printf.sprintf
          {|(library
 (name ffi_lib)
 (modules)
 (libraries %s))
|}
          common_libs
    in
    (* Create executables section *)
    let exec_section =
      Printf.sprintf
        {|(executables
 (names %s)
 (modules %s)
 (libraries %s ffi_lib http_pirc lwt cohttp-lwt-unix yojson)
 (flags (:standard -w -26)))
|}
        executable_names
        executable_names
        common_libs
    in
    (* Write the content to the dune file *)
    let final_content = lib_section ^ "\n" ^ exec_section in
    let oc = open_out dune_path in
    output_string oc final_content;
    close_out oc
  | _ -> invalid_arg "Invalid backend for dune generation"
;;

(* Create path for output files *)
let create_output_path input_file base suffix =
  let output_dir = Filename.dirname input_file in
  Filename.concat output_dir (base ^ suffix)
;;

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

(* Generate code for shared memory backend *)
let generate_shm_code basename locs netir_l =
  let msg_module = (module Codegen.Msg_intf.Msg_chan_intf : Codegen.Msg_intf.M) in
  let out_path = basename ^ ".ml" in
  Codegen.Toplevel_shm.emit_toplevel_shm (open_out out_path) msg_module locs netir_l
;;

(* Generate code for HTTP backend *)
let generate_http_code basename locs netir_l =
  let msg_module = (module Codegen.Msg_intf.Msg_http_intf : Codegen.Msg_intf.M) in
  let examples_dir = "examples" in
  (* Ensure the examples directory exists *)
  if not (Sys.file_exists examples_dir && Sys.is_directory examples_dir)
  then Unix.mkdir examples_dir 0o755;
  (* Extract FFI libraries *)
  let ffi_libs =
    List.fold_left
      (fun acc ir ->
        let files = Ast_utils.collect_ffi_files ir in
        List.fold_left (fun acc' file -> file :: acc') acc files)
      []
      netir_l
    |> List.sort_uniq String.compare
  in
  (* Generate one file per location *)
  let has_ffi_files = List.length ffi_libs > 0 in
  List.iter2
    (fun loc ir ->
      let ml_filename = Printf.sprintf "%s_%s.ml" basename loc in
      let out_path = Filename.concat examples_dir ml_filename in
      let out_file = open_out out_path in
      (* Add appropriate imports *)
      if has_ffi_files
      then output_string out_file "open Http_pirc\nopen Ffi_lib\n\n"
      else output_string out_file "open Http_pirc\n\n";
      Codegen.Toplevel_shm.emit_toplevel_http out_file msg_module [ loc ] [ ir ];
      close_out out_file)
    locs
    netir_l
;;

(* Main entry point *)
let () =
  (* Parse command line arguments *)
  Arg.parse speclist process_input_file usage_msg;
  (* Check if input file was provided *)
  if !file_ic = None || !basename = ""
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);
  (* Create helper for generating output paths *)
  let get_output_path suffix = create_output_path !input_filename !basename suffix in
  (* Parse the input file *)
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.Parse.parse_with_error lexbuf in
  (* Dump the choreography AST *)
  dump_choreo_ast
    !ast_dump_format
    (get_output_path
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
        (get_output_path
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
  (match !backend with
   | "shm" -> generate_shm_code !basename locs netir_l
   | "http" -> generate_http_code !basename locs netir_l
   | _ -> invalid_arg "Invalid backend");
  (* Generate the dune file after generating all ml files *)
  generate_dune_file !basename locs !backend netir_l
;;
