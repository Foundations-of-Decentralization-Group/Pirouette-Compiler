open Ast.Dump
open Ast.Extract
open Parsing.Interface
open Irgen.Epp

let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json>]"
let ast_dump_format = ref ""
let file_ic = ref None
let basename = ref "choreo"

let anon_fun filename =
  basename := Filename.remove_extension filename;
  file_ic := Some (open_in filename)

let () = Printf.printf "The main_exec is being executed\n"

let speclist =
  [
    ( "-ast-dump",
      Arg.Symbol ([ "pprint"; "json" ], fun s -> ast_dump_format := s),
      "Dump the AST in the specified format (pprint, json)" );
    ("-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  Printf.printf "%s \n" !ast_dump_format;
  if !file_ic = None then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);

  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = parse_program lexbuf in

  (match !ast_dump_format with
  | "json" -> jsonify_choreo_ast (open_out (!basename ^ ".json")) program
  | "pprint" -> pprint_choreo_ast (open_out (!basename ^ ".ast")) program
  | _ -> invalid_arg "Invalid ast-dump format");

  let locs = extract_locs program in
  List.iter
    (fun loc ->
      let ir = epp program loc in
      match !ast_dump_format with
      | "json" ->
          jsonify_net_ast (open_out (!basename ^ "." ^ loc ^ ".json")) ir
      | "pprint" ->
          pprint_net_ast (open_out (!basename ^ "." ^ loc ^ ".ast")) ir
      | _ -> invalid_arg "Invalid ast-dump format")
    locs
