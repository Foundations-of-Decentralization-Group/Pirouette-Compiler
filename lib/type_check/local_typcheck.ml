(*dummy metainfo to let compiler happy*)
let m : Ast.Metainfo.metainfo = "", 0, 0, 0

(*context: list of pair of variable name and its binding*)
type local_context = (string * Ast.Local.typ) list
type choreo_context = (string * Ast.Choreo.typ) list
type global_context = (string * string * Ast.Local.typ) list

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*return Result type*)
and ctx_lookup ctx var_name =
  try Ok (List.assoc var_name ctx) with
  | Not_found -> Error "Variable not found"

(*extract loc_id's local context from glocal context*)
and extract_local_ctx global_ctx loc_id =
  List.fold_right
    (fun (loc, var_name, typ) acc -> if loc = loc_id then (var_name, typ) :: acc else acc)
    global_ctx
    []
;;

(* ============================== Local ============================== *)
let rec check_local_expr ctx expected_typ = function
  | Ast.Local.Unit _ -> expected_typ = Ast.Local.TUnit m
  | Val (v, _) -> expected_typ = typeof_Val v
  | Var (VarId (var_name, _), _) ->
    (match ctx_lookup ctx var_name with
     | Ok t -> expected_typ = t
     | _ -> false)
  | UnOp (op, e, _) ->
    (match op with
     | Neg _ -> check_local_expr ctx (TInt m) e && expected_typ = TInt m
     | Not _ -> check_local_expr ctx (TBool m) e && expected_typ = TBool m)
  | BinOp (e1, op, e2, _) ->
    (match op with
     | Plus _ | Minus _ | Times _ | Div _ ->
       check_local_expr ctx (TInt m) e1
       && check_local_expr ctx (TInt m) e2
       && expected_typ = TInt m
     | Eq _ | Neq _ | Lt _ | Leq _ | Gt _ | Geq _ ->
       check_local_expr ctx (TInt m) e1
       && check_local_expr ctx (TInt m) e2
       && expected_typ = TBool m
     | And _ | Or _ ->
       check_local_expr ctx (TBool m) e1
       && check_local_expr ctx (TBool m) e2
       && expected_typ = TBool m)
  | Let (VarId (var_name, _), local_type, e1, e2, _) ->
    (match check_local_expr ctx local_type e1 with
     | true ->
       let ctx' = add_binding ctx var_name local_type in
       check_local_expr ctx' expected_typ e2
     | _ -> false)
  | Pair (e1, e2, _) ->
    (match expected_typ with
     | TProd (t1, t2, _) -> check_local_expr ctx t1 e1 && check_local_expr ctx t2 e2
     | _ -> false)
  | Fst (e, _) ->
    (match expected_typ with
     | TProd (t1, _, _) -> check_local_expr ctx t1 e
     | _ -> false)
  | Snd (e, _) ->
    (match expected_typ with
     | TProd (_, t2, _) -> check_local_expr ctx t2 e
     | _ -> false)
  | Left (e, _) ->
    (match expected_typ with
     | TSum (t1, _, _) -> check_local_expr ctx t1 e
     | _ -> false)
  | Right (e, _) ->
    (match expected_typ with
     | TSum (_, t2, _) -> check_local_expr ctx t2 e
     | _ -> false)
  | Match (_e, _cases, _) -> true

and typeof_Val = function
  | Int _ -> TInt m
  | Bool _ -> TBool m
  | String _ -> TString m
;;
