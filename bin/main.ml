open Ast.Dump
open Parsing.Interface

let usage_msg = "USAGE: pirc <file> [-ast-dump <pprint|json>]"
let ast_dump_format = ref "pprint"
let file_ic = ref None
let anon_fun filename = file_ic := Some (open_in filename)

let speclist =
  [
    ("-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin");
    ( "-ast-dump",
      Arg.Symbol ([ "pprint"; "json" ], fun s -> ast_dump_format := s),
      "Dump the AST in the specified format (pprint, json)" );
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;

  if !file_ic = None then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    exit 1);

  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = parse_program lexbuf in
  match !ast_dump_format with
  | "json" -> jsonify_choreo_ast Format.std_formatter program
  | "pprint" -> pprint_choreo_ast Format.std_formatter program
  | _ -> failwith "Invalid ast-dump format"
