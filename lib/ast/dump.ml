open Dump_json
open Dump_pprint

let jsonify_choreo_ast ppf (Choreo.Prog prog) = Yojson.Basic.pretty_print ppf (jsonify_stmt_block prog)

let pprint_choreo_ast ppf (Choreo.Prog prog) = pprint_choreo_stmt_block ppf prog; Format.pp_print_newline ppf ()

let pprint_net_ast ppf (Net.Prog prog) = pprint_net_stmt_block ppf prog; Format.pp_print_newline ppf ()
