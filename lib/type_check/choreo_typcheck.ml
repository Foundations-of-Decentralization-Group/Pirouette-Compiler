open Local_typcheck

let rec check_stmts stmts expected_typ =
  let choreo_ctx = [] in
  let global_ctx = [] in
  List.for_all (fun stmt -> check_stmt choreo_ctx global_ctx expected_typ stmt) stmts

and check_stmt choreo_ctx global_ctx expected_typ = function
  | Ast.Choreo.Decl (_pattn, _choreo_typ, _) -> true
  | Assign (_pattn_ls, expr, _) ->
    check_choreo_expr choreo_ctx global_ctx expected_typ expr
  | TypeDecl (TypId (_id, _), _choreo_typ, _) -> true

and check_choreo_expr choreo_ctx global_ctx expected_typ = function
  | Unit _ -> expected_typ = Ast.Choreo.TUnit m
  | Var (VarId (var_name, _), _) ->
    (match ctx_lookup choreo_ctx var_name with
     | Ok t -> expected_typ = t
     | _ -> false)
  | LocExpr (LocId (loc_id, _), e, _) ->
    (match expected_typ with
     | TLoc (LocId (loc_id', _), local_typ, _) ->
       loc_id = loc_id'
       && check_local_expr (extract_local_ctx global_ctx loc_id) local_typ e
     | _ -> false)
  | Send (LocId (loc_id1, _), e, LocId (loc_id2, _), _) ->
    (match expected_typ with
     | TLoc (LocId (loc_id2', _), local_typ, _) ->
       check_choreo_expr choreo_ctx global_ctx (TLoc (LocId (loc_id1, m), local_typ, m)) e
       && loc_id2 = loc_id2'
     | _ -> false)
  | Sync (_, _, _, e, _) -> check_choreo_expr choreo_ctx global_ctx expected_typ e
  | If (cond, c1, c2, _) ->
    (match cond with
     | LocExpr (loc_id, _, _) ->
       check_choreo_expr choreo_ctx global_ctx expected_typ c1
       && check_choreo_expr choreo_ctx global_ctx expected_typ c2
       && check_choreo_expr choreo_ctx global_ctx (TLoc (loc_id, TBool m, m)) cond
     | _ -> false)
  | Pair (e1, e2, _) ->
    (match expected_typ with
     | TProd (t1, t2, _) ->
       check_choreo_expr choreo_ctx global_ctx t1 e1
       && check_choreo_expr choreo_ctx global_ctx t2 e2
     | _ -> false)
  | Fst (e, _) ->
    (match expected_typ with
     | TProd (t1, _, _) -> check_choreo_expr choreo_ctx global_ctx t1 e
     | _ -> false)
  | Snd (e, _) ->
    (match expected_typ with
     | TProd (_, t2, _) -> check_choreo_expr choreo_ctx global_ctx t2 e
     | _ -> false)
  | Left (e, _) ->
    (match expected_typ with
     | TSum (t1, _, _) -> check_choreo_expr choreo_ctx global_ctx t1 e
     | _ -> false)
  | Right (e, _) ->
    (match expected_typ with
     | TSum (_, t2, _) -> check_choreo_expr choreo_ctx global_ctx t2 e
     | _ -> false)
  | Match (_e, _ls, _) -> true
  | Let (_stmt_block, _e, _) -> true
  | FunDef (_pattern_ls, _e, _) -> true
  | FunApp (_e1, _e2, _) -> true
;;
