module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

let rec collect_data
          (p : 'a Choreo.stmt list)
          (holder1 : string list)
          (_holder2 : string list)
  : string list
  =
  match p with
  | [] -> []
  | head :: tail ->
    let list_one = copy_matcher_stmt head holder1 _holder2 in
    collect_data tail list_one _holder2

and copy_matcher_stmt
      (x : 'a Choreo.stmt)
      (holder1 : string list)
      (_holder2 : string list)
  : string list
  =
  match x with
  | Decl (_, _, _) -> []
  | Assign (_, e, _) ->
    (* this is where we go to the choreo expr variant*)
    copy_choreo_expr e holder1 _holder2 (* print_endline "Hit the Assign AST node" *)
  | TypeDecl (_, _, _) -> []

and copy_choreo_expr (x : 'a Choreo.expr) (holder1 : string list) (_holder2 : string list)
  : string list
  =
  match x with
  | Unit _ -> []
  | Var (_, _) -> []
  | LocExpr (_, _, _) -> []
  | Send (_, e, _, _) -> copy_choreo_expr e holder1 _holder2
  | If (e1, e2, e3, _) ->
    let list_two =
      let list_one = copy_choreo_expr e1 holder1 _holder2 in
      copy_choreo_expr e2 list_one _holder2
    in
    copy_choreo_expr e3 list_two _holder2
  | Let (stmts, e, _) ->
    let list_one = collect_data stmts holder1 _holder2 in
    copy_choreo_expr e list_one _holder2
  | FunDef (_, e, _) -> copy_choreo_expr e holder1 _holder2
  | FunApp (e1, e2, _) ->
    let list_one = copy_choreo_expr e1 holder1 _holder2 in
    copy_choreo_expr e2 list_one _holder2
  | Pair (e1, e2, _) ->
    let list_one = copy_choreo_expr e1 holder1 _holder2 in
    copy_choreo_expr e2 list_one _holder2
  | Fst (e, _) -> copy_choreo_expr e holder1 _holder2
  | Snd (e, _) -> copy_choreo_expr e holder1 _holder2
  | Left (e, _) -> copy_choreo_expr e holder1 _holder2
  | Right (e, _) -> copy_choreo_expr e holder1 _holder2
  | Match (e, _, _) -> copy_choreo_expr e holder1 _holder2
  | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
    print_endline "Hit the Sync Ast node";
    Printf.printf "These are required strings %s %s\n" id1 id3;
    let holder1 = List.cons id1 holder1 in
    let holder1 = List.cons id3 holder1 in
    let rec check_consecutive_syncs (e : 'a Choreo.expr) holder1 =
      match e with
      | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
        Printf.printf "These are required strings %s %s \n" id1 id3;
        let holder1 = List.cons id1 holder1 in
        let holder1 = List.cons id3 holder1 in        
        check_consecutive_syncs e holder1;
      | _ -> holder1
    in
    let final_net_count = check_consecutive_syncs e holder1 in
    final_net_count
;;
