open Dump_json
open Dump_pprint

let jsonify_choreo_ast channel (Choreo.Prog prog) = 
  Yojson.Basic.pretty_to_channel channel (jsonify_choreo_stmt_block prog)

let jsonify_net_ast channel (Net.Prog prog) =
  Yojson.Basic.pretty_to_channel channel (jsonify_net_stmt_block prog)

let pprint_choreo_ast channel (Choreo.Prog prog) =
  let ppf = Format.formatter_of_out_channel channel in
  pprint_choreo_stmt_block ppf prog;
  Format.pp_print_newline ppf ()

let pprint_net_ast channel (Net.Prog prog) =
  let ppf = Format.formatter_of_out_channel channel in
  pprint_net_stmt_block ppf prog;
  Format.pp_print_newline ppf ()