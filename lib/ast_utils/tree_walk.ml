module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

let rec matcher_local_expr (x : 'a Local.expr) (holder : string list) : string list =
  match x with
  | Unit _ ->
    print_endline "Hit a Local Unit AST Node";
    holder
  | Val (v, _) ->
    print_endline "Hit a Local Val Ast Node";
    matcher_local_val v;
    holder
  | Var (v, _) ->
    print_endline "Hit a Local Var Ast Node";
    match_loc_var_id v;
    holder
  | UnOp (u, e, _) ->
    print_endline "Hit a Local Unop Ast Node";
    matcher_local_unop u;
    matcher_local_expr e holder
  | BinOp (e1, bop, e2, _) ->
    print_endline "Hit a Local Binop Node";
    matcher_local_binop bop;
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Let (v, e1, e2, _) ->
    print_endline "Hit a Local Let AST node";
    match_loc_var_id v;
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Pair (e1, e2, _) ->
    print_endline "Hit a Local Pair AST node";
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Fst (e, _) ->
    print_endline "Hit a Local Fst AST node";
    matcher_local_expr e holder
  | Snd (e, _) ->
    print_endline "Hit a Local Snd AST node";
    matcher_local_expr e holder
  | Left (e, _) ->
    print_endline "Hit a Local Left AST node";
    matcher_local_expr e holder
  | Right (e, _) ->
    print_endline "Hit a Local Right AST node";
    matcher_local_expr e holder
  | Match (e, l, _) ->
    print_endline "Hit a Local Match AST node";
    let holder = matcher_local_expr e holder in
    matcher_local_case l holder

and matcher_local_val (x : 'a Local.value) =
  match x with
  | Int (_, _) -> print_endline "Hit a Local Int AST node"
  | String (_, _) -> print_endline "Hit a Local String AST node"
  | Bool (_, _) -> print_endline "Encountered a Local Bool AST node"

and match_loc_var_id (x : 'a Local.var_id) =
  match x with
  | VarId (_, _) -> print_endline "Hit a local Var ID AST node"

and matcher_local_unop (x : 'a Local.un_op) =
  match x with
  | Not _ -> print_endline "Hit a local Not Unop AST node"
  | Neg _ -> print_endline "Hit a local Neg Unop AST node"

and matcher_local_binop (x : 'a Local.bin_op) =
  match x with
  | Plus _ -> print_endline "Hit a Local Binop Node of type plus"
  | Minus _ -> print_endline "Hit a Local Binop Node of type minus"
  | Times _ -> print_endline "Hit a Local Binop Node of type times"
  | Div _ -> print_endline "Hit a Local Binop Node of type division"
  | And _ -> print_endline "Hit a Local Binop Node of type and"
  | Or _ -> print_endline "Hit a Local Binop Node of type or"
  | Eq _ -> print_endline "Hit a Local Binop Node of type eq"
  | Neq _ -> print_endline "Hit a Local Binop Node of type Neq"
  | Lt _ -> print_endline "Hit a Local Binop Node of type Lt"
  | Leq _ -> print_endline "Hit a Local Binop Node of type Leq"
  | Gt _ -> print_endline "Hit a Local Binop Node of type Gt"
  | Geq _ -> print_endline "Hit a Local Binop Node of type Geq"

and matcher_local_case
      (x : ('a Local.pattern * 'a Local.expr) list)
      (holder : string list)
  : string list
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    holder
  | head :: tail ->
    let holder = split_local_pat_expr head holder in
    matcher_local_case tail holder

and split_local_pat_expr (x : 'a Local.pattern * 'a Local.expr) (holder : string list)
  : string list
  =
  match x with
  | p, e ->
    let holder = match_loc_pattern p holder in
    matcher_local_expr e holder

and match_loc_pattern (x : 'a Local.pattern) (holder : string list) : string list =
  match x with
  | Default _ ->
    print_endline "Hit a local Default pattern AST node";
    holder
  | Val (v, _) ->
    print_endline "Hit a local Val pattern AST node";
    matcher_local_val v;
    holder
  | Var (v, _) ->
    print_endline "Hit a local Var pattern AST node";
    match_loc_var_id v;
    holder
  | Pair (p1, p2, _) ->
    print_endline "Hit a local Pair pattern AST node";
    let holder = match_loc_pattern p1 holder in
    match_loc_pattern p2 holder
  | Left (p, _) ->
    print_endline "Hit a local Left pattern AST node";
    match_loc_pattern p holder
  | Right (p, _) ->
    print_endline "Hit a local Right pattern AST node";
    match_loc_pattern p holder
;;

let rec get_stmt_block (p : 'a Choreo.stmt list) (holder : string list) : string list =
  match p with
  | [] ->
    print_endline "Done with the statement list";
    holder
  | head :: tail ->
    let holder = matcher_stmt head holder in
    get_stmt_block tail holder

and matcher_stmt (x : 'a Choreo.stmt) (holder : string list) : string list =
  match x with
  | Decl (p, t, _) ->
    print_endline "Hit the Decl AST node";
    let holder1 = matcher_pattern p holder in
    matcher_choreo_typ t holder1
  | Assign (ps, e, _) ->
    print_endline "Hit the Assign AST node";
    let holder1 = get_pattern_list ps holder in
    matcher_choreo_expr e holder1
  | TypeDecl (t1, t2, _) ->
    print_endline "Hit the TypeDecl AST node";
    let holder1 = matcher_loc_typ_id t1 holder in
    matcher_choreo_typ t2 holder1

and matcher_choreo_expr (x : 'a Choreo.expr) (holder : string list) : string list =
  match x with
  | Unit _ ->
    print_endline "Hit the Unit AST node in Choreo";
    holder
  | Var _ ->
    print_endline "Hit the Var AST node in Choreo";
    holder
  | LocExpr (l, e, _) ->
    print_endline "Hit the LocExpr AST node\n";
    let holder = matcher_loc_id l holder in
    matcher_local_expr e holder
  | Send (l1, e, l2, _) ->
    print_endline "Hit the Send Ast node\n";
    let final_holder =
      let holder = matcher_loc_id l1 holder in
      matcher_choreo_expr e holder
    in
    matcher_loc_id l2 final_holder
    (* matcher_loc_id l2 holder *)
  | Sync (l1, l2, l3, e, _) ->
    print_endline "Hit the Sync Ast node";
    matcher_sync_label_id l2;
    let holder2 =
      let holder1 = matcher_loc_id l1 holder in
      matcher_loc_id l3 holder1
    in
    matcher_choreo_expr e holder2
    (* matcher_choreo_expr e *)
  | If (e1, e2, e3, _) ->
    print_endline "Hit the If AST node";
    let holder2 =
      let holder1 = matcher_choreo_expr e1 holder in
      matcher_choreo_expr e2 holder1
    in
    matcher_choreo_expr e3 holder2
    (* matcher_choreo_expr e2; *)
    (* matcher_choreo_expr e3 *)
  | Let (stmts, e, _) ->
    print_endline "Hit the Let AST node";
    let holder1 = get_stmt_block stmts holder in
    matcher_choreo_expr e holder1
  | FunDef (ps, e, _) ->
    print_endline "Hit the FunDef AST node";
    let holder1 = get_pattern_list ps holder in
    matcher_choreo_expr e holder1
  | FunApp (e1, e2, _) ->
    print_endline "Hit the FunApp AST node";
    let holder = matcher_choreo_expr e1 holder in
    matcher_choreo_expr e2 holder
  | Pair (e1, e2, _) ->
    print_endline "Hit the Pair AST node";
    let holder = matcher_choreo_expr e1 holder in
    matcher_choreo_expr e2 holder
  | Fst (e, _) ->
    print_endline "Hit the FST AST node";
    matcher_choreo_expr e holder
  | Snd (e, _) ->
    print_endline "Hit the Snd AST node";
    matcher_choreo_expr e holder
  | Left (e, _) ->
    print_endline "Hit the Left AST node";
    matcher_choreo_expr e holder
  | Right (e, _) ->
    print_endline "Hit the Right AST node";
    matcher_choreo_expr e holder
  | Match (e, cases, _) ->
    print_endline "Hit the Match AST node";
    let holder1 = matcher_choreo_expr e holder in
    matcher_choreo_case cases holder1

and matcher_choreo_typ (x : 'a Choreo.typ) (holder : string list) : string list =
  match x with
  | TUnit _ ->
    print_endline "Hit the Choreo typ's TUnit AST node";
    holder
  | TLoc (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TLoc AST node";
    let holder = matcher_loc_id t1 holder in
    matcher_local_typ t2 holder
  | TMap (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TMap AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder
  | TProd (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TProd AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder
  | TSum (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TSum AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder

and matcher_local_typ (x : 'a Local.typ) (holder : string list) =
  match x with
  | TUnit _ ->
    print_endline "Hit the Local typ's TUnit AST node";
    holder
  | TInt _ ->
    print_endline "Hit the Local typ's TInt AST node";
    holder
  | TString _ ->
    print_endline "Hit the Local typ's TString AST node";
    holder
  | TBool _ ->
    print_endline "Hit the Local typ's TBool AST node";
    holder
  | TProd (t1, t2, _) ->
    print_endline "Hit the Local typ's TProd AST node";
    let holder1 = matcher_local_typ t1 holder in
    matcher_local_typ t2 holder1
  | TSum (t1, t2, _) ->
    print_endline "Hit the Local typ's TSum AST node";
    let holder1 = matcher_local_typ t1 holder in
    matcher_local_typ t2 holder1

and matcher_loc_id (x : 'a Local.loc_id) (holder : string list) : string list =
  match x with
  | LocId (id, _) ->
    print_endline "Hit the Loc Id AST node";
    Printf.printf "This is the location %s\n" id;
    let holder = List.cons id holder in
    holder

and matcher_loc_typ_id (x : 'a Local.typ_id) (holder : string list) =
  match x with
  | TypId (_, _) ->
    print_endline "Hit the Typ ID Ast node";
    holder

and matcher_sync_label_id (x : 'a Local.sync_label) =
  match x with
  | LabelId (_, _) -> print_endline "Hit the Sync Label Id AST node"

and matcher_choreo_case
      (x : ('a Choreo.pattern * 'a Choreo.expr) list)
      (holder : string list)
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    holder
  | head :: tail ->
    let holder1 = split_choreo_pat_expr head holder in
    matcher_choreo_case tail holder1

and split_choreo_pat_expr (x : 'a Choreo.pattern * 'a Choreo.expr) (holder : string list)
  : string list
  =
  match x with
  | p, e ->
    let holder1 = matcher_pattern p holder in
    matcher_choreo_expr e holder1

and matcher_pattern (x : 'a Choreo.pattern) (holder : string list) : string list =
  match x with
  | Default _ ->
    print_endline "Hit a Default AST node";
    holder
  | Var _ ->
    print_endline "Hit a Var AST node";
    holder
  | Pair (p1, p2, _) ->
    print_endline "Hit a Pair AST node";
    let holder = matcher_pattern p1 holder in
    matcher_pattern p2 holder
  | LocPat (l1, l2, _) ->
    Printf.printf "Hit a LocalPattern AST node\n";
    let holder = matcher_loc_id l1 holder in
    match_loc_pattern l2 holder
  | Left (p, _) ->
    print_endline "Hit a Left AST node";
    matcher_pattern p holder
  | Right (p, _) ->
    print_endline "Hit a Right AST node";
    matcher_pattern p holder

and get_pattern_list (p : 'a Choreo.pattern list) (holder : string list) =
  match p with
  | [] ->
    print_endline "Done with the pattern list";
    holder
  | head :: tail ->
    let holder1 = matcher_pattern head holder in
    get_pattern_list tail holder1
;;

let rec copy_local_expr (x : 'a Local.expr) (holder : string list) : 'a Local.expr =
  match x with
  | Unit a ->
    print_endline "Hit a Local Unit AST Node";
    Unit a
  | Val (v, a) ->
    print_endline "Hit a Local Val Ast Node";
    Val (copy_local_val v, a)
  | Var (v, a) ->
    print_endline "Hit a Local Var Ast Node";
    Var (copy_loc_var_id v, a)
  | UnOp (u, e, a) ->
    print_endline "Hit a Local Unop Ast Node";
    UnOp (copy_local_unop u, copy_local_expr e holder, a)
  | BinOp (e1, bop, e2, a) ->
    print_endline "Hit a Local Binop Node";
    BinOp (copy_local_expr e1 holder, copy_local_binop bop, copy_local_expr e2 holder, a)
  | Let (v, e1, e2, a) ->
    print_endline "Hit a Local Let AST node";
    Let (copy_loc_var_id v, copy_local_expr e1 holder, copy_local_expr e2 holder, a)
  | Pair (e1, e2, a) ->
    print_endline "Hit a Local Pair AST node";
    Pair (copy_local_expr e1 holder, copy_local_expr e2 holder, a)
  | Fst (e, a) ->
    print_endline "Hit a Local Fst AST node";
    Fst (copy_local_expr e holder, a)
  | Snd (e, a) ->
    print_endline "Hit a Local Snd AST node";
    Snd (copy_local_expr e holder, a)
  | Left (e, a) ->
    print_endline "Hit a Local Left AST node";
    Left (copy_local_expr e holder, a)
  | Right (e, a) ->
    print_endline "Hit a Local Right AST node";
    Right (copy_local_expr e holder, a)
  | Match (e, l, a) ->
    print_endline "Hit a Local Match AST node";
    Match (copy_local_expr e holder, copy_local_case l holder, a)

and copy_local_val (x : 'a Local.value) : 'a Local.value =
  match x with
  | Int (i, a) ->
    print_endline "Hit a Local Int AST node";
    Int (i, a)
  | String (s, a) ->
    print_endline "Hit a Local String AST node";
    String (s, a)
  | Bool (b, a) ->
    print_endline "Encountered a Local Bool AST node";
    Bool (b, a)

and copy_loc_var_id (x : 'a Local.var_id) : 'a Local.var_id =
  match x with
  | VarId (s, a) ->
    print_endline "Hit a local Var ID AST node";
    VarId (s, a)

and copy_local_unop (x : 'a Local.un_op) : 'a Local.un_op =
  match x with
  | Not a ->
    print_endline "Hit a local Not Unop AST node";
    Not a
  | Neg a ->
    print_endline "Hit a local Neg Unop AST node";
    Neg a

and copy_local_binop (x : 'a Local.bin_op) : 'a Local.bin_op =
  match x with
  | Plus a ->
    print_endline "Hit a Local Binop Node of type plus";
    Plus a
  | Minus a ->
    print_endline "Hit a Local Binop Node of type minus";
    Minus a
  | Times a ->
    print_endline "Hit a Local Binop Node of type times";
    Times a
  | Div a ->
    print_endline "Hit a Local Binop Node of type division";
    Div a
  | And a ->
    print_endline "Hit a Local Binop Node of type and";
    And a
  | Or a ->
    print_endline "Hit a Local Binop Node of type or";
    Or a
  | Eq a ->
    print_endline "Hit a Local Binop Node of type eq";
    Eq a
  | Neq a ->
    print_endline "Hit a Local Binop Node of type Neq";
    Neq a
  | Lt a ->
    print_endline "Hit a Local Binop Node of type Lt";
    Lt a
  | Leq a ->
    print_endline "Hit a Local Binop Node of type Leq";
    Leq a
  | Gt a ->
    print_endline "Hit a Local Binop Node of type Gt";
    Gt a
  | Geq a ->
    print_endline "Hit a Local Binop Node of type Geq";
    Geq a

and copy_local_case (x : ('a Local.pattern * 'a Local.expr) list) (holder : string list)
  : ('a Local.pattern * 'a Local.expr) list
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    []
  | head :: tail -> copy_split_local_pat_expr head holder :: copy_local_case tail holder

and copy_split_local_pat_expr
      (x : 'a Local.pattern * 'a Local.expr)
      (holder : string list)
  : 'a Local.pattern * 'a Local.expr
  =
  match x with
  | p, e -> copy_loc_pattern p holder, copy_local_expr e holder

and copy_loc_pattern (x : 'a Local.pattern) (holder : string list) : 'a Local.pattern =
  match x with
  | Default a ->
    print_endline "Hit a local Default pattern AST node";
    Default a
  | Val (v, a) ->
    print_endline "Hit a local Val pattern AST node";
    Val (copy_local_val v, a)
  | Var (v, a) ->
    print_endline "Hit a local Var pattern AST node";
    Var (copy_loc_var_id v, a)
  | Pair (p1, p2, a) ->
    print_endline "Hit a local Pair pattern AST node";
    Pair (copy_loc_pattern p1 holder, copy_loc_pattern p2 holder, a)
  | Left (p, a) ->
    print_endline "Hit a local Left pattern AST node";
    Left (copy_loc_pattern p holder, a)
  | Right (p, a) ->
    print_endline "Hit a local Right pattern AST node";
    Right (copy_loc_pattern p holder, a)
;;

let rec copy_get_stmt_block (p : 'a Choreo.stmt list) (holder : string list)
  : 'a Choreo.stmt list
  =
  match p with
  | [] ->
    print_endline "Done with the statement list";
    []
  | head :: tail -> copy_matcher_stmt head holder :: copy_get_stmt_block tail holder

and copy_matcher_stmt (x : 'a Choreo.stmt) (holder : string list) : 'a Choreo.stmt =
  match x with
  | Decl (p, t, a) ->
    print_endline "Hit the Decl AST node";
    Decl (copy_choreo_pattern p holder, copy_choreo_typ t holder, a)
  | Assign (ps, e, a) ->
    print_endline "Hit the Assign AST node";
    Assign (copy_get_pattern_list ps holder, copy_choreo_expr e holder, a)
  | TypeDecl (t1, t2, a) ->
    print_endline "Hit the TypeDecl AST node";
    TypeDecl (copy_loc_typ_id t1 holder, copy_choreo_typ t2 holder, a)

and get_guard_loc_copy_expr (x : 'a Choreo.expr) : string =
  match x with
  | LocExpr (l, _, _) ->
    print_endline "Hit the LocExpr AST node\n";
    get_copy_loc_id l
  | _ -> String.empty

and add_sync_copy_choreo_expr
      (e : 'a Choreo.expr)
      (holder : string list)
      (label : string)
      (s : string)
      a
  : 'a Choreo.expr
  =
  let l1 = s in
  let l2 = label in
  let rec deconstruct_list s holder a x : 'a Choreo.expr =
    match holder with
    | [] -> x
    | head :: tail ->
      if String.equal l1 head
      then deconstruct_list s tail a x
      else (
        print_endline "Ran once";
        let l3 = head in
        (* Sync *)
        (*   ( LocId (l1, a) *)
        (*   , LabelId (l2, a) *)
        (*   , LocId (l3, a) *)
        (*   , Sync *)
        (*       ( LocId (l1, a) *)
        (*       , LabelId (l2, a) *)
        (*       , LocId (l3, a) *)
        (*       , deconstruct_list s tail a x *)
        (*       , a ) *)
        (*   , a )) *)
        Sync
          (LocId (l1, a), LabelId (l2, a), LocId (l3, a), deconstruct_list s tail a x, a))
  in
  deconstruct_list s holder a e

and copy_choreo_expr (x : 'a Choreo.expr) (holder : string list) : 'a Choreo.expr =
  match x with
  | Unit a ->
    print_endline "Hit the Unit AST node in Choreo";
    Unit a
  | Var (v, a) ->
    print_endline "Hit the Var AST node in Choreo";
    Var (copy_loc_var_id v, a)
  | LocExpr (l, e, a) ->
    print_endline "Hit the LocExpr AST node\n";
    LocExpr (copy_loc_id l holder, copy_local_expr e holder, a)
  | Send (l1, e, l2, a) ->
    print_endline "Hit the Send Ast node\n";
    Send (copy_loc_id l1 holder, copy_choreo_expr e holder, copy_loc_id l2 holder, a)
  | Sync (l1, l2, l3, e, a) ->
    print_endline "Hit the Sync Ast node";
    Sync
      ( copy_loc_id l1 holder
      , copy_sync_label_id l2
      , copy_loc_id l3 holder
      , copy_choreo_expr e holder
      , a )
  | If (e1, e2, e3, a) ->
    (* Have to add in a clause to take the normal If if the number of participants is 1 *)
    print_endline "Hit the If AST node";
    let s = get_guard_loc_copy_expr e1 in
    let left_label = "L" in
    let right_label = "R" in
    Printf.printf "############# This is the Loc ID %s ###########\n" s;
    If
      ( copy_choreo_expr e1 holder
      , add_sync_copy_choreo_expr e2 holder left_label s a
      , add_sync_copy_choreo_expr e3 holder right_label s a
      , a )
    (* If *)
    (*   ( copy_choreo_expr e1 holder *)
    (*   , copy_choreo_expr e2 holder *)
    (*   , copy_choreo_expr e3 holder *)
    (*   , a ) *)
  | Let (stmts, e, a) ->
    print_endline "Hit the Let AST node";
    Let (copy_get_stmt_block stmts holder, copy_choreo_expr e holder, a)
  | FunDef (ps, e, a) ->
    print_endline "Hit the FunDef AST node";
    FunDef (copy_get_pattern_list ps holder, copy_choreo_expr e holder, a)
  | FunApp (e1, e2, a) ->
    print_endline "Hit the FunApp AST node";
    FunApp (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a)
  | Pair (e1, e2, a) ->
    print_endline "Hit the Pair AST node";
    Pair (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a)
  | Fst (e, a) ->
    print_endline "Hit the FST AST node";
    Fst (copy_choreo_expr e holder, a)
  | Snd (e, a) ->
    print_endline "Hit the Snd AST node";
    Snd (copy_choreo_expr e holder, a)
  | Left (e, a) ->
    print_endline "Hit the Left AST node";
    Left (copy_choreo_expr e holder, a)
  | Right (e, a) ->
    print_endline "Hit the Right AST node";
    Right (copy_choreo_expr e holder, a)
  | Match (e, cases, a) ->
    print_endline "Hit the Match AST node";
    Match (copy_choreo_expr e holder, copy_choreo_case cases holder, a)

(* This is a copy in case mistakes happen *)
(* and copy_choreo_expr (x : 'a Choreo.expr) (holder : string list) : 'a Choreo.expr = *)
(*   match x with *)
(*   | Unit a -> *)
(*     print_endline "Hit the Unit AST node in Choreo"; *)
(*     Unit a *)
(*   | Var (v, a) -> *)
(*     print_endline "Hit the Var AST node in Choreo"; *)
(*     Var (copy_loc_var_id v, a) *)
(*   | LocExpr (l, e, a) -> *)
(*     print_endline "Hit the LocExpr AST node\n"; *)
(*     LocExpr (copy_loc_id l holder, copy_local_expr e holder, a) *)
(*   | Send (l1, e, l2, a) -> *)
(*     print_endline "Hit the Send Ast node\n"; *)
(*     Send (copy_loc_id l1 holder, copy_choreo_expr e holder, copy_loc_id l2 holder, a) *)
(*   | Sync (l1, l2, l3, e, a) -> *)
(*     print_endline "Hit the Sync Ast node"; *)
(*     Sync *)
(*       ( copy_loc_id l1 holder *)
(*       , copy_sync_label_id l2 *)
(*       , copy_loc_id l3 holder *)
(*       , copy_choreo_expr e holder *)
(*       , a ) *)
(*   | If (e1, e2, e3, a) -> *)
(*     print_endline "Hit the If AST node"; *)
(*     If *)
(*       ( copy_choreo_expr e1 holder *)
(*       , copy_choreo_expr e2 holder *)
(*       , copy_choreo_expr e3 holder *)
(*       , a ) *)
(*   | Let (stmts, e, a) -> *)
(*     print_endline "Hit the Let AST node"; *)
(*     Let (copy_get_stmt_block stmts holder, copy_choreo_expr e holder, a) *)
(*   | FunDef (ps, e, a) -> *)
(*     print_endline "Hit the FunDef AST node"; *)
(*     FunDef (copy_get_pattern_list ps holder, copy_choreo_expr e holder, a) *)
(*   | FunApp (e1, e2, a) -> *)
(*     print_endline "Hit the FunApp AST node"; *)
(*     FunApp (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a) *)
(*   | Pair (e1, e2, a) -> *)
(*     print_endline "Hit the Pair AST node"; *)
(*     Pair (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a) *)
(*   | Fst (e, a) -> *)
(*     print_endline "Hit the FST AST node"; *)
(*     Fst (copy_choreo_expr e holder, a) *)
(*   | Snd (e, a) -> *)
(*     print_endline "Hit the Snd AST node"; *)
(*     Snd (copy_choreo_expr e holder, a) *)
(*   | Left (e, a) -> *)
(*     print_endline "Hit the Left AST node"; *)
(*     Left (copy_choreo_expr e holder, a) *)
(*   | Right (e, a) -> *)
(*     print_endline "Hit the Right AST node"; *)
(*     Right (copy_choreo_expr e holder, a) *)
(*   | Match (e, cases, a) -> *)
(*     print_endline "Hit the Match AST node"; *)
(*     Match (copy_choreo_expr e holder, copy_choreo_case cases holder, a) *)

and copy_choreo_typ (x : 'a Choreo.typ) (holder : string list) : 'a Choreo.typ =
  match x with
  | TUnit a ->
    print_endline "Hit the Choreo typ's TUnit AST node";
    TUnit a
  | TLoc (t1, t2, a) ->
    print_endline "Hit the Choreo typ's TLoc AST node";
    TLoc (copy_loc_id t1 holder, copy_local_typ t2 holder, a)
  | TMap (t1, t2, a) ->
    print_endline "Hit the Choreo typ's TMap AST node";
    TMap (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)
  | TProd (t1, t2, a) ->
    print_endline "Hit the Choreo typ's TProd AST node";
    TProd (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)
  | TSum (t1, t2, a) ->
    print_endline "Hit the Choreo typ's TSum AST node";
    TSum (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)

and copy_local_typ (x : 'a Local.typ) (holder : string list) =
  match x with
  | TUnit a ->
    print_endline "Hit the Local typ's TUnit AST node";
    TUnit a
  | TInt a ->
    print_endline "Hit the Local typ's TInt AST node";
    TInt a
  | TString a ->
    print_endline "Hit the Local typ's TString AST node";
    TString a
  | TBool a ->
    print_endline "Hit the Local typ's TBool AST node";
    TBool a
  | TProd (t1, t2, a) ->
    print_endline "Hit the Local typ's TProd AST node";
    TProd (copy_local_typ t1 holder, copy_local_typ t2 holder, a)
  | TSum (t1, t2, a) ->
    print_endline "Hit the Local typ's TSum AST node";
    TSum (copy_local_typ t1 holder, copy_local_typ t2 holder, a)

and copy_loc_id (x : 'a Local.loc_id) (_holder : string list) : 'a Local.loc_id =
  match x with
  | LocId (id, a) ->
    print_endline "Hit the Loc Id AST node";
    LocId (id, a)

and get_copy_loc_id (x : 'a Local.loc_id) : string =
  match x with
  | LocId (id, _) ->
    print_endline "Hit the Loc Id AST node";
    id

and copy_loc_typ_id (x : 'a Local.typ_id) (_holder : string list) : 'a Local.typ_id =
  match x with
  | TypId (s, a) ->
    print_endline "Hit the Typ ID Ast node";
    TypId (s, a)

and copy_sync_label_id (x : 'a Local.sync_label) : 'a Local.sync_label =
  match x with
  | LabelId (s, a) ->
    print_endline "Hit the Sync Label Id AST node";
    LabelId (s, a)

and copy_choreo_case
      (x : ('a Choreo.pattern * 'a Choreo.expr) list)
      (holder : string list)
  : ('a Choreo.pattern * 'a Choreo.expr) list
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    []
  | head :: tail -> copy_split_choreo_pat_expr head holder :: copy_choreo_case tail holder

and copy_split_choreo_pat_expr
      (x : 'a Choreo.pattern * 'a Choreo.expr)
      (holder : string list)
  =
  match x with
  | p, e -> copy_choreo_pattern p holder, copy_choreo_expr e holder

(* Need a good return from this guy ; matcher_pattern is copy_choreo_pattern *)
and copy_choreo_pattern (x : 'a Choreo.pattern) (holder : string list) : 'a Choreo.pattern
  =
  match x with
  | Default a ->
    print_endline "Hit a Default AST node";
    Default a
  | Var (v, a) ->
    print_endline "Hit a Var AST node";
    Var (copy_loc_var_id v, a)
  | Pair (p1, p2, a) ->
    print_endline "Hit a Pair AST node";
    Pair (copy_choreo_pattern p1 holder, copy_choreo_pattern p2 holder, a)
  | LocPat (l1, l2, a) ->
    print_endline "Hit a LocalPattern AST node";
    LocPat (copy_loc_id l1 holder, copy_loc_pattern l2 holder, a)
  | Left (p, a) ->
    print_endline "Hit a Left AST node";
    Left (copy_choreo_pattern p holder, a)
  | Right (p, a) ->
    print_endline "Hit a Right AST node";
    Right (copy_choreo_pattern p holder, a)

and copy_get_pattern_list (p : 'a Choreo.pattern list) (holder : string list) =
  match p with
  | [] ->
    print_endline "Done with the pattern list";
    []
  | head :: tail -> copy_choreo_pattern head holder :: copy_get_pattern_list tail holder
;;

(* This is for adding the batch *)
