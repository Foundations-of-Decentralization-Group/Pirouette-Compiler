let rec ast_alpha_rename : 'a Ast_core.Choreo.M.stmt -> 'a Ast_core.Choreo.M.stmt = function  
  | Decl (stm_pattern, stmt_type, metadata) -> Decl (stm_pattern, stmt_type, metadata)
  | Assign (stmt_pattern_list, stmt_expr, metadata) -> Assign (stmt_pattern_list, stmt_expr, metadata)
  | TypeDecl (TypId (name, meta), stmt_type, metadata) -> TypeDecl (TypId (name ^ "_pirar", meta), stmt_type, metadata)
  | ForeignDecl (VarId (name, meta), stmt_type, stmt_foreign_str, metadata) -> ForeignDecl (VarId (name ^ "_pirar", meta), stmt_type, stmt_foreign_str, metadata)

  in
  
let rec ast_list_alpha_rename : 'a Ast_core.Choreo.M.stmt_block -> 'a Ast_core.Choreo.M.stmt_block = function
  | [] -> []
  | h::d -> ((ast_alpha_rename h) :: (ast_list_alpha_rename d))
;;