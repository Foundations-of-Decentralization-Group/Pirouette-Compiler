module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M
module Parsed_choreo = Parsing.Parsed_ast.Choreo

let extract_locs (stmt_block : 'a Choreo.stmt_block) =
  Ast_locs.extract_stmt_block stmt_block |> Ast_locs.LocSet.elements
;;

let stringify_jsonify_choreo_ast (stmt_block : 'a Choreo.stmt_block) =
  Jsonify_ast.jsonify_choreo_stmt_block stmt_block |> Yojson.Basic.pretty_to_string
;;

let stringify_jsonify_net_ast (stmt_block : 'a Net.stmt_block) =
  Jsonify_ast.jsonify_net_stmt_block stmt_block |> Yojson.Basic.pretty_to_string
;;

let jsonify_choreo_ast out_chan (stmt_block : 'a Choreo.stmt_block) =
  Jsonify_ast.jsonify_choreo_stmt_block stmt_block
  |> Yojson.Basic.pretty_to_channel out_chan
;;

let jsonify_net_ast out_chan (stmt_block : 'a Net.stmt_block) =
  Jsonify_ast.jsonify_net_stmt_block stmt_block |> Yojson.Basic.pretty_to_channel out_chan
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

let stringify_dot_choreo_ast program = Choreo_dot.generate_dot_code program

let dot_choreo_ast out_chan (stmt_block : Parsed_choreo.stmt_block) =
  let dot_code = stringify_dot_choreo_ast stmt_block in
  output_string out_chan dot_code;
  flush out_chan
;;
