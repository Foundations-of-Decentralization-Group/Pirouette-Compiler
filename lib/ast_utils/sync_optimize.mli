val collect_data
  :  'a Ast_core.Choreo.M.stmt list
  -> string list * 'a Ast_core.Choreo.M.expr list
  -> string list * 'a Ast_core.Choreo.M.expr list

val batch_reorder
  :  'a Ast_core.Choreo.M.stmt list
  -> 'a Ast_core.Choreo.M.expr list    
  -> string list
  -> 'a Ast_core.Choreo.M.stmt list
