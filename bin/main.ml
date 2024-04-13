open Ast.Dump
open Ast.Print
open Parsing.Interface

let usage_msg = "USAGE: pirc <file> [-ast-dump]"
let ast_dump = ref false
let file_ic = ref stdin

let anon_fun filename =
  file_ic := open_in filename

let speclist = [("-ast-dump", Arg.Set ast_dump, "Dump the AST as JSON")]

let () =
  Arg.parse speclist anon_fun usage_msg;

  let lexbuf = Lexing.from_channel !file_ic in
  let program = parse_program lexbuf in
  if !ast_dump then
    dump_choreo_ast Format.std_formatter program
  else
    print_choreo_ast Format.std_formatter program
