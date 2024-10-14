let choreo_typ_check (Ast.Choreo.Prog (stmts, _)) expected_typ =
  Choreo_typcheck.check_stmts stmts expected_typ
;;
