let usage_msg : string = "append [-verbose] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""
let crazy_symbol = ref ""
let anon_fun filename = input_files := filename :: !input_files

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
    ( "-cs",
      Arg.Symbol ([ "pprint"; "json" ], fun s -> crazy_symbol := s),
      "Dump the AST in the specified format (pprint, json)" );
  ]

let () =
  (* This parses the command line input*)
  Arg.parse speclist anon_fun usage_msg;
  Printf.printf "%b \n" !verbose;
  Printf.printf "%s \n" !output_file;
  Printf.printf "%s \n" !crazy_symbol
(* Main functionality here *)
