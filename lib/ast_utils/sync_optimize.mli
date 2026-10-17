val collect_data
  :  'a Ast_core.Choreo.M.stmt list
  -> string list * 'a Ast_core.Local.M.value list
  -> string list * 'a Ast_core.Local.M.value list

val add_sync_opt
  :  'a Ast_core.Choreo.M.stmt list
  -> string list
  -> 'a Ast_core.Choreo.M.stmt list
