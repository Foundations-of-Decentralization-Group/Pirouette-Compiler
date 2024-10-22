val node_counter : int ref
val generate_node_name : unit -> string
val string_of_value : Parsing.Parsed_ast.Local.value -> string
val dot_bin_op : Parsing.Parsed_ast.Local.bin_op -> string * string
val dot_un_op : Parsing.Parsed_ast.Local.un_op -> string * string
val dot_local_type : Parsing.Parsed_ast.Local.typ -> string * string
val dot_local_pattern : Parsing.Parsed_ast.Local.pattern -> string * string
val dot_local_expr : Parsing.Parsed_ast.Local.expr -> string * string
