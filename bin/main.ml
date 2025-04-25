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
let generate_dune_file base_name locs backend netir_l =
  (* Extract FFI libraries from all network ASTs *)
  let ffi_libs = 
    List.fold_left (fun acc ir ->
      let files = Ast_utils.collect_ffi_files ir in
      List.fold_left (fun acc' file -> file :: acc') acc files
    ) [] netir_l
    |> List.sort_uniq String.compare
    |> List.map Filename.basename
    |> List.map (fun file -> 
       (* Remove .ml extension for module names in dune file *)
       if Filename.check_suffix file ".ml" then
         Filename.remove_extension file
       else 
         file)
  in
  
  let common_libs = "ast_core parsing codegen ast_utils netgen ppxlib" in
  (* Add FFI libraries if they exist *)
  let library_deps = 
    if List.length ffi_libs > 0 then
      common_libs ^ " " ^ String.concat " " ffi_libs
    else
      common_libs
  in
  
  match backend with
  | "shm" ->
    (* For SHM backend, create a new dune file in the root *)
    let content = Printf.sprintf
      {|(executable
 (name %s)
 (libraries %s domainslib))
|}
      base_name (* Use the extracted base name *)
      library_deps
    in
    let oc = open_out "dune" in
    output_string oc content;
    close_out oc

  | "http" ->
    (* For HTTP backend, create a new dune file in examples/ directory *)
    let dune_path = "examples/dune" in
    let executable_names =
      List.map (fun loc -> Printf.sprintf "%s_%s" base_name loc) locs (* Use the extracted base name *)
      |> String.concat " "
    in
      
    (* Create library section for FFI files if they exist *)
    let lib_section = 
      if List.length ffi_libs > 0 then
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
    let exec_section = Printf.sprintf
      {|(executables
 (names %s)
 (modules %s)
 (libraries %s ffi_lib http_pirc lwt cohttp-lwt-unix yojson)
 (flags (:standard -w -26)))
|}
      executable_names
      executable_names  (* Use executable names as module names too *)
      common_libs
    in
      
    (* Create complete dune file content *)
    let final_content = lib_section ^ "\n" ^ exec_section in
      
    (* Write the content to the dune file, overwriting any existing file *)
    let oc = open_out dune_path in
    output_string oc final_content;
    close_out oc

  | _ -> invalid_arg "Invalid backend for dune generation"
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

    (* Extract FFI libraries from all network ASTs *)
    let ffi_libs = 
      List.fold_left (fun acc ir ->
        let files = Ast_utils.collect_ffi_files ir in
        List.fold_left (fun acc' file -> file :: acc') acc files
      ) [] netir_l
      |> List.sort_uniq String.compare
    in
    
    (* Determine if we have any FFI files *)
    let has_ffi_files = List.length ffi_libs > 0 in

    List.iter2
      (fun loc ir ->
        (* Use the extracted base name here *)
        let ml_filename = Printf.sprintf "%s_%s.ml" !basename loc in
        let out_path = Filename.concat examples_dir ml_filename in (* Output to examples/ *)
        let out_file = open_out out_path in
        (* Only add open Ffi_lib if FFI files are used *)
        if has_ffi_files then
          output_string out_file "open Http_pirc\nopen Ffi_lib\n\n"
        else
          output_string out_file "open Http_pirc\n\n";
        Codegen.Toplevel_shm.emit_toplevel_http out_file msg_module [ loc ] [ ir ];
        close_out out_file)
      locs
      netir_l
  | _ -> invalid_arg "Invalid backend");

  (* Generate the dune file after generating all ml files *)
  generate_dune_file !basename locs !backend netir_l
;;
