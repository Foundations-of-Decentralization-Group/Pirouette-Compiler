module LocSet = Set.Make (String)

let rec extract_stmt_block (stmts : Choreo.stmt_block) =
  List.fold_left
    (fun acc stmt -> LocSet.union acc (extract_stmt stmt))
    LocSet.empty stmts

and extract_stmt = function
  | Decl (p, t) -> LocSet.union (extract_pattern p) (extract_type t)
  | Assign (ps, e) -> LocSet.union (List.fold_left (fun acc p -> LocSet.union acc (extract_pattern p)) LocSet.empty ps) (extract_expr e)
  | TypeDecl (_, t) -> extract_type t

and extract_expr = function
  | Unit | Var _ -> LocSet.empty
  | LocExpr (LocId id, _) -> LocSet.singleton id
  | Send (LocId id1, e, LocId id2) -> LocSet.add id2 (LocSet.add id1 (extract_expr e))
  | Sync (LocId id1, _, LocId id2, e) ->
      LocSet.add id2 (LocSet.add id1 (extract_expr e))
  | If (e1, e2, e3) ->
      LocSet.union (extract_expr e1)
        (LocSet.union (extract_expr e2) (extract_expr e3))
  | Let (stmts, e) -> LocSet.union (extract_stmt_block stmts) (extract_expr e)
  | FunDef (p, e) -> LocSet.union (extract_pattern p) (extract_expr e)
  | FunApp (e1, e2) | Pair (e1, e2) -> LocSet.union (extract_expr e1) (extract_expr e2)
  | Fst e | Snd e | Left e | Right e -> extract_expr e
  | Match (e, cases) ->
      List.fold_left
        (fun acc (p, e) ->
          LocSet.union acc (LocSet.union (extract_pattern p) (extract_expr e)))
        (extract_expr e) cases

and extract_pattern = function
  | Default | Var _ -> LocSet.empty
  | Pair (p1, p2) -> LocSet.union (extract_pattern p1) (extract_pattern p2)
  | LocPatt (LocId id, _) -> LocSet.singleton id
  | Left p | Right p -> extract_pattern p

and extract_type = function
  | TUnit -> LocSet.empty
  | TLoc (LocId id, _) -> LocSet.singleton id
  | TMap (t1, t2) | TProd (t1, t2) | TSum (t1, t2) -> LocSet.union (extract_type t1) (extract_type t2)

let extract_locs (Choreo.Prog stmts) = extract_stmt_block stmts |> LocSet.elements
