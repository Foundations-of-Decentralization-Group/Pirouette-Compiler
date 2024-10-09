val extract_locs : Ast.Choreo.program -> String.t list
val jsonify_choreo_ast : out_channel -> Ast.Choreo.program -> unit
val jsonify_net_ast : out_channel -> Ast.Net.program -> unit
val pprint_choreo_ast : out_channel -> Ast.Choreo.program -> unit
val pprint_net_ast : out_channel -> Ast.Net.program -> unit
