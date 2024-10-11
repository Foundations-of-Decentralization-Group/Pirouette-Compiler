val extract_locs : Ast.Choreo.stmt_block -> String.t list
val jsonify_choreo_ast : out_channel -> Ast.Choreo.stmt_block -> unit
val jsonify_net_ast : out_channel -> Ast.Net.stmt_block -> unit
val pprint_choreo_ast : out_channel -> Ast.Choreo.stmt_block -> unit
val pprint_net_ast : out_channel -> Ast.Net.stmt_block -> unit
