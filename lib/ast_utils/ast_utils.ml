module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

let extract_locs (stmt_block : 'a Choreo.stmt_block) =
  Ast_locs.extract_stmt_block stmt_block |> Ast_locs.LocSet.elements
;;

let jsonify_choreo_ast out_chan (stmt_block : 'a Choreo.stmt_block) =
  Yojson.Basic.pretty_to_channel
    out_chan
    (Jsonify_ast.jsonify_choreo_stmt_block stmt_block)
;;

let jsonify_net_ast out_chan (stmt_block : 'a Net.stmt_block) =
  Yojson.Basic.pretty_to_channel out_chan (Jsonify_ast.jsonify_net_stmt_block stmt_block)
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
