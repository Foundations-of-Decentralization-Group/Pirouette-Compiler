val extract_locs : 'a Ast_core.Choreo.M.stmt_block -> string list
val tree_walk : 'a Ast_core.Choreo.M.stmt list -> string list -> string list
val infer_sync_expr : 'a Ast_core.Choreo.M.stmt list -> string list -> 'a Ast_core.Choreo.M.stmt list     
val jsonify_choreo_ast : out_channel -> 'a Ast_core.Choreo.M.stmt_block -> unit
val jsonify_net_ast : out_channel -> 'a Ast_core.Net.M.stmt_block -> unit
val pprint_choreo_ast : out_channel -> 'a Ast_core.Choreo.M.stmt_block -> unit
val pprint_net_ast : out_channel -> 'a Ast_core.Net.M.stmt_block -> unit
