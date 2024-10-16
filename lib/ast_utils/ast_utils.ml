let extract_locs (Ast.Choreo.Prog stmts) =
  Ast_locs.extract_stmt_block stmts |> Ast_locs.LocSet.elements
;;

let jsonify_choreo_ast channel (Ast.Choreo.Prog prog) =
  Yojson.Basic.pretty_to_channel channel (Jsonify_ast.jsonify_choreo_stmt_block prog)
;;

let jsonify_net_ast channel (Ast.Net.Prog prog) =
  Yojson.Basic.pretty_to_channel channel (Jsonify_ast.jsonify_net_stmt_block prog)
;;

let pprint_choreo_ast channel (Ast.Choreo.Prog prog) =
  let ppf = Format.formatter_of_out_channel channel in
  Pprint_ast.pprint_choreo_stmt_block ppf prog;
  Format.pp_print_newline ppf ()
;;

let pprint_net_ast channel (Ast.Net.Prog prog) =
  let ppf = Format.formatter_of_out_channel channel in
  Pprint_ast.pprint_net_stmt_block ppf prog;
  Format.pp_print_newline ppf ()
;;
