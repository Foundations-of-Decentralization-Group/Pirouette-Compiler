module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

let extract_locs (stmt_block : 'a Choreo.stmt_block) =
  Ast_locs.extract_stmt_block stmt_block |> Ast_locs.LocSet.elements
;;

let stringify_jsonify_choreo_ast (stmt_block : 'a Choreo.stmt_block) =
  Jsonify_ast.jsonify_choreo_stmt_block stmt_block |> Yojson.Basic.pretty_to_string
;;

let stringify_jsonify_net_ast (stmt_block : 'a Net.stmt_block) =
  Jsonify_ast.jsonify_net_stmt_block stmt_block |> Yojson.Safe.pretty_to_string
;;

let jsonify_choreo_ast out_chan (stmt_block : 'a Choreo.stmt_block) =
  Jsonify_ast.jsonify_choreo_stmt_block stmt_block
  |> Yojson.Basic.pretty_to_channel out_chan
;;

let jsonify_net_ast out_chan (stmt_block : 'a Net.stmt_block) =
  Jsonify_ast.jsonify_net_stmt_block stmt_block |> Yojson.Safe.pretty_to_channel out_chan
;;

let stringify_pprint_choreo_ast (stmt_block : 'a Choreo.stmt_block) =
  Pprint_ast.pprint_choreo_stmt_block Format.str_formatter stmt_block;
  Format.flush_str_formatter ()
;;

let stringify_pprint_net_ast (stmt_block : 'a Net.stmt_block) =
  Pprint_ast.pprint_net_stmt_block Format.str_formatter stmt_block;
  Format.flush_str_formatter ()
;;

let pprint_choreo_ast out_chan (stmt_block : 'a Choreo.stmt_block) =
  let ppf = Format.formatter_of_out_channel out_chan in
  Pprint_ast.pprint_choreo_stmt_block ppf stmt_block;
  Format.pp_print_newline ppf ()
;;

let pprint_net_ast out_chan (stmt_block : 'a Net.stmt_block) =
  let ppf = Format.formatter_of_out_channel out_chan in
  Pprint_ast.pprint_net_stmt_block ppf stmt_block;
  Format.pp_print_newline ppf ()
;;

let stringify_dot_choreo_ast (string_of_info : 'a -> string) program =
  Dot_ast.generate_dot_code string_of_info program
;;

let dot_choreo_ast
  out_chan
  (string_of_info : 'a -> string)
  (stmt_block : 'a Choreo.stmt_block)
  =
  let dot_code = stringify_dot_choreo_ast string_of_info stmt_block in
  output_string out_chan dot_code;
  flush out_chan
;;

(* FFI (Foreign Function Interface) utilities *)

(* Extract the module file path from a foreign declaration string *)
let extract_ffi_file external_name =
  if String.starts_with ~prefix:"@" external_name then
    match String.split_on_char ':' (String.sub external_name 1 (String.length external_name - 1)) with
    | [file; _] when file <> "" -> Some file
    | _ -> None
  else
    None
;;

(* Extract all unique FFI file references from a list of statements *)
let collect_ffi_files stmts =
  let rec collect acc = function
    | [] -> acc
    | Net.ForeignDecl (_, _, external_name, _) :: rest ->
        let acc' = match extract_ffi_file external_name with
          | Some file -> file :: acc
          | None -> acc
        in
        collect acc' rest
    | _ :: rest -> collect acc rest
  in
  collect [] stmts |> List.sort_uniq String.compare
;;

(* Generate dune libraries string from FFI files *)
let generate_ffi_libraries stmts =
  let files = collect_ffi_files stmts in
  String.concat " " (List.map (fun file -> 
    Filename.basename file |> Filename.remove_extension
  ) files)
;;
