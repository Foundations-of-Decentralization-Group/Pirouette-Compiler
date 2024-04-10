open Ast

let rec epp_stmt (stmt : Choreo.stmt) (loc : string) : Net.stmt =
  match stmt with
  | Decl (p, t) -> Decl (epp_pattern p loc, epp_type t loc)
  | Assign (p, e) -> Assign (epp_pattern p loc, epp_expr e loc)
  | TypeDecl (id, t) -> TypeDecl (id, epp_type t loc)

and epp_expr (e : Choreo.expr) (loc : string) : Net.expr =
  match e with
  | LocExpr (LocId loc1, e) when loc1 == loc -> Ret e
  | FunDef (p, e) -> FunDef (epp_pattern p loc, epp_expr e loc)
  | FunApp (e1, e2) -> FunApp (epp_expr e1 loc, epp_expr e2 loc)
  | Pair (e1, e2) -> Pair (epp_expr e1 loc, epp_expr e2 loc)
  | Fst e -> Fst (epp_expr e loc)
  | Snd e -> Snd (epp_expr e loc)
  | Left e -> Left (epp_expr e loc)
  | Right e -> Right (epp_expr e loc)
  | Let (stmts, e) ->
    Let (List.map (fun stmt -> epp_stmt stmt loc) stmts, epp_expr e loc)
  (* | Send (e, LocId loc1) ->
      if loc1 = loc then 
      else SendTo (epp_expr e loc, LocId loc1) *)
  | Sync (LocId id1, l, LocId id2, e) ->
      if id1 = loc && id2 != loc then ChooseFor (l, LocId id2, epp_expr e loc)
      else if id2 = loc && id1 != loc then AllowChoice (LocId id1, [ (l, epp_expr e loc) ])
      else if id1 != id2 then epp_expr e loc
      else Unit
  | If (e1, e2, e3) -> (
      match merge_expr (epp_expr e2 loc) (epp_expr e3 loc) with
      | Some e -> e
      | None -> If (epp_expr e1 loc, epp_expr e2 loc, epp_expr e3 loc))
  | Match (e, cases) -> (
      let merged_cases =
        List.fold_left (fun acc (_, e) ->
            match acc with
            | Some acc -> merge_expr acc (epp_expr e loc)
            | None -> None)
          (Some Unit) (List.rev cases)
      in
      match merged_cases with
      | Some e -> e
      | None -> 
          Match
            ( epp_expr e loc,
              List.map (fun (p, e) -> (epp_pattern p loc, epp_expr e loc)) cases
            ))
  | _ -> Unit

and epp_pattern (p : Choreo.pattern) (loc : string) : Local.pattern =
  match p with
  | Pair (p1, p2) -> Pair (epp_pattern p1 loc, epp_pattern p2 loc)
  | LocPatt (LocId id, p) -> if id = loc then p else Default
  | Left p -> Left (epp_pattern p loc)
  | Right p -> Right (epp_pattern p loc)
  | _ -> Default

and epp_type (t : Choreo.typ) (loc : string) : Net.typ =
  match t with
  | TLoc (LocId loc1, t1) -> if loc1 = loc then TLoc t1 else TUnit
  | TMap (t1, t2) -> TMap (epp_type t1 loc, epp_type t2 loc)
  | TProd (t1, t2) -> TProd (epp_type t1 loc, epp_type t2 loc)
  | TSum (t1, t2) -> TSum (epp_type t1 loc, epp_type t2 loc)
  | _ -> TUnit

and merge_expr (e1 : Net.expr) (e2 : Net.expr) : Net.expr option =
  match (e1, e2) with
  | Unit, Unit -> Some Unit
  | Var id1, Var id2 when id1 = id2 -> Some (Var id1)
  | Ret e, Ret e' when e = e' -> Some (Ret e)
  | FunDef (p, e), FunDef (p', e') when p = p' && e = e' -> Some (FunDef (p, e))
  | FunApp (e1, e2), FunApp (e1', e2') -> (
      match merge_expr e1 e1', merge_expr e2 e2' with
      | Some e1, Some e2 -> Some (FunApp (e1, e2))
      | _ -> None)
  | Send (e1, LocId loc), Send (e2, LocId loc') when loc = loc' -> (
      match merge_expr e1 e2 with
      | Some e -> Some (Send (e, LocId loc))
      | None -> None)
  | Recv (LocId loc), Recv (LocId loc') when loc = loc' -> Some (Recv (LocId loc))
  | If (e1, e2, e3), If (e1', e2', e3') when e1 = e1' -> (
      match merge_expr e2 e2', merge_expr e3 e3' with
      | Some e2, Some e3 -> Some (If (e1, e2, e3))
      | _ -> None)
  | Let (stmts1, e1), Let (stmts2, e2) -> (
      let merged_bindings =
        List.concat_map (fun stmt1 ->
            List.filter_map (fun stmt2 -> merge_stmt stmt1 stmt2) stmts2) stmts1
      in
      match (merged_bindings, merge_expr e1 e2) with
      | stmts, Some e -> Some (Let (stmts, e))
      | _ -> None)
  | ChooseFor (l, LocId loc, e1), ChooseFor (l', LocId loc', e2) when l = l' && loc = loc' -> (
      match merge_expr e1 e2 with
      | Some e -> Some (ChooseFor (l, LocId loc, e))
      | None -> None)
  | AllowChoice (LocId loc, choices1), AllowChoice (LocId loc', choices2) when loc = loc' ->
      Some
        (AllowChoice
           ( LocId loc,
             let h = Hashtbl.create 2 in
             List.iter (merge_choice_into h) choices1;
             List.iter (merge_choice_into h) choices2;
             Hashtbl.fold (fun l e acc -> (l, e) :: acc) h [] ))
  | _ -> None

and merge_stmt (stmt1 : Net.stmt) (stmt2 : Net.stmt) : Net.stmt option =
  match stmt1, stmt2 with
  | Decl (p, t), Decl (p', t') when p = p' && t = t' -> Some (Decl (p, t))
  | TypeDecl (id, t), TypeDecl (id', t') when id = id' && t = t' -> Some (TypeDecl (id, t))
  | Assign (p, e1), Assign (p', e2) when p = p' -> (
      match merge_expr e1 e2 with
      | Some e -> Some (Assign (p, e))
      | None -> None)
  | _ -> None

and merge_choice_into tbl (label, expr) =
  match Hashtbl.find_opt tbl label with
  | Some expr' -> (
      match merge_expr expr expr' with
      | Some e -> Hashtbl.replace tbl label e
      | None -> Hashtbl.add tbl label expr')
  | None -> Hashtbl.add tbl label expr