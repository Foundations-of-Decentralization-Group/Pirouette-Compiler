val extract_locs : Ast_core.Choreo.M.stmt_block -> string list
val jsonify_choreo_ast : out_channel -> Ast_core.Choreo.M.stmt_block -> unit
val jsonify_net_ast : out_channel -> Ast_core.Net.M.stmt_block -> unit
val pprint_choreo_ast : out_channel -> Ast_core.Choreo.M.stmt_block -> unit
val pprint_net_ast : out_channel -> Ast_core.Net.M.stmt_block -> unit
