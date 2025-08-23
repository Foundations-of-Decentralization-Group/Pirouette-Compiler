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
  | [] -> holder1
  | head :: tail ->
    let list_two =
      let list_one = copy_matcher_stmt head holder1 _holder2 in
      collect_data tail list_one _holder2
    in
    list_two

and copy_matcher_stmt
      (x : 'a Choreo.stmt)
      (holder1 : string list)
      (_holder2 : string list)
  : string list
  =
  match x with
  | Assign (_, e, _) ->
    (* this is where we go to the choreo expr variant*)
    let rs_list = visit_choreo_expr e holder1 _holder2 in
    rs_list
  | _ ->
    print_endline "Hit this";
    []

and visit_choreo_expr
      (x : 'a Choreo.expr)
      (holder1 : string list)
      (_holder2 : string list)
  : string list
  =
  match x with
  | Unit _ -> holder1
  | Var (_, _) -> holder1
  | LocExpr (_, _, _) -> holder1
  | Send (_, e, _, _) -> visit_choreo_expr e holder1 _holder2
  | If (e1, e2, _, _) ->
    (* let list_three = *)
    (*   let list_two = *)
    (*     let list_one = visit_choreo_expr e1 holder1 _holder2 in *)
    (*     visit_choreo_expr e2 list_one _holder2 *)
    (*   in *)
    (*   visit_choreo_expr e3 list_two _holder2 *)
    (* in *)
    (* (\* let size = List.length list_three in *\) *)
    (* Printf.printf "This is the size of the internal list %d \n" size; *)
    (* list_three *)
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Let (stmts, e, _) ->
    let list_one = collect_data stmts holder1 _holder2 in
    visit_choreo_expr e list_one _holder2
  | FunDef (_, e, _) -> visit_choreo_expr e holder1 _holder2
  | FunApp (e1, e2, _) ->
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Pair (e1, e2, _) ->
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Fst (e, _) -> visit_choreo_expr e holder1 _holder2
  | Snd (e, _) -> visit_choreo_expr e holder1 _holder2
  | Left (e, _) -> visit_choreo_expr e holder1 _holder2
  | Right (e, _) -> visit_choreo_expr e holder1 _holder2
  | Match (e, _, _) -> visit_choreo_expr e holder1 _holder2
  | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
    print_endline "Hit the Sync Ast node";
    let holder1 = List.cons id1 holder1 in
    let holder1 = List.cons id3 holder1 in
    let rec check_consecutive_syncs (e : 'a Choreo.expr) holder1 =
      match e with
      | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
        let holder1 = List.cons id1 holder1 in
        let holder1 = List.cons id3 holder1 in
        check_consecutive_syncs e holder1
      | _ -> holder1
    in
    let list_to_be_reversed = check_consecutive_syncs e holder1 in
    let final_list = List.rev list_to_be_reversed in
    let rec check_optimization_possible input_list output_list switch =
      match input_list with
      | [] -> []
      | head :: tail ->
        if switch mod 2 = 0
        then head :: check_optimization_possible tail output_list (switch + 1)
        else check_optimization_possible tail output_list (switch + 1)
    in
    if List.length final_list = 0
    then (
      print_endline
        "Cannot be optimized because there are no sync statements/control messages";
      [])
    else (
      let result_list = check_optimization_possible final_list [] 0 in
      let rec check_equality input_list acc =
        match input_list with
        | [] -> true
        | head :: tail -> if String.equal head acc then check_equality tail acc else false
      in
      let enable_opt = check_equality (List.tl result_list) (List.hd result_list) in
      let final_list = List.cons (Bool.to_string enable_opt) final_list in
      if enable_opt
      then print_endline "Optimization possible"
      else
        print_endline
          "Optimization not possible because the sender for each of the control messages \
           is different";
      final_list)
;;

(* This is a marker that denotes the part of the file used to check if optimization is a good idea - the said portion is above*)

let rec copy_local_expr (x : 'a Local.expr) (holder : string list) : 'a Local.expr =
  match x with
  | Unit a ->
    (* print_endline "Hit a Local Unit AST Node"; *)
    Unit a
  | Val (v, a) ->
    (* print_endline "Hit a Local Val Ast Node"; *)
    Val (copy_local_val v, a)
  | Var (v, a) ->
    (* print_endline "Hit a Local Var Ast Node"; *)
    Var (copy_loc_var_id v, a)
  | UnOp (u, e, a) ->
    (* print_endline "Hit a Local Unop Ast Node"; *)
    UnOp (copy_local_unop u, copy_local_expr e holder, a)
  | BinOp (e1, bop, e2, a) ->
    (* print_endline "Hit a Local Binop Node"; *)
    BinOp (copy_local_expr e1 holder, copy_local_binop bop, copy_local_expr e2 holder, a)
  | Let (v, e1, e2, a) ->
    (* print_endline "Hit a Local Let AST node"; *)
    Let (copy_loc_var_id v, copy_local_expr e1 holder, copy_local_expr e2 holder, a)
  | Pair (e1, e2, a) ->
    (* print_endline "Hit a Local Pair AST node"; *)
    Pair (copy_local_expr e1 holder, copy_local_expr e2 holder, a)
  | Fst (e, a) ->
    (* print_endline "Hit a Local Fst AST node"; *)
    Fst (copy_local_expr e holder, a)
  | Snd (e, a) ->
    (* print_endline "Hit a Local Snd AST node"; *)
    Snd (copy_local_expr e holder, a)
  | Left (e, a) ->
    (* print_endline "Hit a Local Left AST node"; *)
    Left (copy_local_expr e holder, a)
  | Right (e, a) ->
    (* print_endline "Hit a Local Right AST node"; *)
    Right (copy_local_expr e holder, a)
  | Match (e, l, a) ->
    (* print_endline "Hit a Local Match AST node"; *)
    Match (copy_local_expr e holder, copy_local_case l holder, a)

and copy_local_val (x : 'a Local.value) : 'a Local.value =
  match x with
  | Int (i, a) ->
    (* print_endline "Hit a Local Int AST node"; *)
    Int (i, a)
  | String (s, a) ->
    (* print_endline "Hit a Local String AST node"; *)
    String (s, a)
  | Bool (b, a) ->
    (* print_endline "Encountered a Local Bool AST node"; *)
    Bool (b, a)

and copy_loc_var_id (x : 'a Local.var_id) : 'a Local.var_id =
  match x with
  | VarId (s, a) ->
    (* print_endline "Hit a local Var ID AST node"; *)
    VarId (s, a)

and copy_local_unop (x : 'a Local.un_op) : 'a Local.un_op =
  match x with
  | Not a ->
    (* print_endline "Hit a local Not Unop AST node"; *)
    Not a
  | Neg a ->
    (* print_endline "Hit a local Neg Unop AST node"; *)
    Neg a

and copy_local_binop (x : 'a Local.bin_op) : 'a Local.bin_op =
  match x with
  | Plus a ->
    (* print_endline "Hit a Local Binop Node of type plus"; *)
    Plus a
  | Minus a ->
    (* print_endline "Hit a Local Binop Node of type minus"; *)
    Minus a
  | Times a ->
    (* print_endline "Hit a Local Binop Node of type times"; *)
    Times a
  | Div a ->
    (* print_endline "Hit a Local Binop Node of type division"; *)
    Div a
  | And a ->
    (* print_endline "Hit a Local Binop Node of type and"; *)
    And a
  | Or a ->
    (* print_endline "Hit a Local Binop Node of type or"; *)
    Or a
  | Eq a ->
    (* print_endline "Hit a Local Binop Node of type eq"; *)
    Eq a
  | Neq a ->
    (* print_endline "Hit a Local Binop Node of type Neq"; *)
    Neq a
  | Lt a ->
    (* print_endline "Hit a Local Binop Node of type Lt"; *)
    Lt a
  | Leq a ->
    (* print_endline "Hit a Local Binop Node of type Leq"; *)
    Leq a
  | Gt a ->
    (* print_endline "Hit a Local Binop Node of type Gt"; *)
    Gt a
  | Geq a ->
    (* print_endline "Hit a Local Binop Node of type Geq"; *)
    Geq a

and copy_local_case (x : ('a Local.pattern * 'a Local.expr) list) (holder : string list)
  : ('a Local.pattern * 'a Local.expr) list
  =
  match x with
  | [] ->
    (* print_endline "Done with the list of cases"; *)
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
    (* print_endline "Hit a local Default pattern AST node"; *)
    Default a
  | Val (v, a) ->
    (* print_endline "Hit a local Val pattern AST node"; *)
    Val (copy_local_val v, a)
  | Var (v, a) ->
    (* print_endline "Hit a local Var pattern AST node"; *)
    Var (copy_loc_var_id v, a)
  | Pair (p1, p2, a) ->
    (* print_endline "Hit a local Pair pattern AST node"; *)
    Pair (copy_loc_pattern p1 holder, copy_loc_pattern p2 holder, a)
  | Left (p, a) ->
    (* print_endline "Hit a local Left pattern AST node"; *)
    Left (copy_loc_pattern p holder, a)
  | Right (p, a) ->
    (* print_endline "Hit a local Right pattern AST node"; *)
    Right (copy_loc_pattern p holder, a)
;;

let rec add_sync_opt (p : 'a Choreo.stmt list) (holder : string list)
  : 'a Choreo.stmt list
  =
  match p with
  | [] ->
    (* print_endline "Done with the statement list"; *)
    []
  | head :: tail -> copy_matcher_stmt head holder :: add_sync_opt tail holder

and copy_matcher_stmt (x : 'a Choreo.stmt) (holder : string list) : 'a Choreo.stmt =
  match x with
  | Decl (p, t, a) ->
    (* print_endline "Hit the Decl AST node"; *)
    Decl (copy_choreo_pattern p holder, copy_choreo_typ t holder, a)
  | Assign (ps, e, a) ->
    (* print_endline "Hit the Assign AST node"; *)
    Assign (copy_get_pattern_list ps holder, copy_choreo_expr e holder, a)
  | TypeDecl (t1, t2, a) ->
    (* print_endline "Hit the TypeDecl AST node"; *)
    TypeDecl (copy_loc_typ_id t1 holder, copy_choreo_typ t2 holder, a)

and copy_choreo_expr (x : 'a Choreo.expr) (holder : string list) : 'a Choreo.expr =
  match x with
  | Unit a ->
    (* print_endline "Hit the Unit AST node in Choreo"; *)
    Unit a
  | Var (v, a) ->
    (* print_endline "Hit the Var AST node in Choreo"; *)
    Var (copy_loc_var_id v, a)
  | LocExpr (l, e, a) ->
    (* print_endline "Hit the LocExpr AST node\n"; *)
    LocExpr (copy_loc_id l holder, copy_local_expr e holder, a)
  | Send (l1, e, l2, a) ->
    (* print_endline "Hit the Send Ast node\n"; *)
    Send (copy_loc_id l1 holder, copy_choreo_expr e holder, copy_loc_id l2 holder, a)
  | Sync (l1, l2, l3, e, a) ->
    print_endline "In the default";
    (* print_endline "Hit the Sync Ast node"; *)
    Sync
      ( copy_loc_id l1 holder
      , copy_sync_label_id l2
      , copy_loc_id l3 holder
      , copy_choreo_expr e holder
      , a )
  | If (e1, e2, e3, a) ->
    (match e2 with
     | Sync (_, _, _, _, _) ->
       print_endline "In here";
       If
         ( copy_choreo_expr e1 holder
         , copy_choreo_expr_left_sync e2 holder 0 a
         , copy_choreo_expr_right_sync e3 holder 0 a
         , a )
     | _ ->
       If
         ( copy_choreo_expr e1 holder
         , copy_choreo_expr e2 holder
         , copy_choreo_expr e3 holder
         , a ))
  | Let (stmts, e, a) ->
    (* print_endline "Hit the Let AST node"; *)
    Let (add_sync_opt stmts holder, copy_choreo_expr e holder, a)
  | FunDef (ps, e, a) ->
    (* print_endline "Hit the FunDef AST node"; *)
    FunDef (copy_get_pattern_list ps holder, copy_choreo_expr e holder, a)
  | FunApp (e1, e2, a) ->
    (* print_endline "Hit the FunApp AST node"; *)
    FunApp (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a)
  | Pair (e1, e2, a) ->
    (* print_endline "Hit the Pair AST node"; *)
    Pair (copy_choreo_expr e1 holder, copy_choreo_expr e2 holder, a)
  | Fst (e, a) ->
    (* print_endline "Hit the FST AST node"; *)
    Fst (copy_choreo_expr e holder, a)
  | Snd (e, a) ->
    (* print_endline "Hit the Snd AST node"; *)
    Snd (copy_choreo_expr e holder, a)
  | Left (e, a) ->
    (* print_endline "Hit the Left AST node"; *)
    Left (copy_choreo_expr e holder, a)
  | Right (e, a) ->
    (* print_endline "Hit the Right AST node"; *)
    Right (copy_choreo_expr e holder, a)
  | Match (e, cases, a) ->
    (* print_endline "Hit the Match AST node"; *)
    Match (copy_choreo_expr e holder, copy_choreo_case cases holder, a)

(* and copy_choreo_expr_right_sync (x : 'a Choreo.expr) (holder : string list) a *)
(*   : 'a Choreo.expr *)
(*   = *)
(*   match x with *)
(*   | Sync (l1, l2, l3, e, a) -> *)
(*     print_endline "Hit the Sync Right Ast node"; *)
(*     Sync *)
(*       ( copy_loc_id l1 holder *)
(*       , copy_sync_label_id l2 *)
(*       , copy_loc_id l3 holder *)
(*       , copy_choreo_expr e holder *)
(*       , a ) *)
(*   | _ -> *)
(*     print_endline "This is being used"; *)
(*     Unit a *)

and copy_choreo_expr_right_sync (x : 'a Choreo.expr) (holder : string list) (index : int) a
  : 'a Choreo.expr
  =
  print_endline "Inside here ie the right sync expr";
  match x with
  | Sync (_, _, _, e, a) ->
    print_endline "Hit the Sync Right Ast node";
    (match e with
     | Sync (_, _, _, _, _) ->
       print_endline "Hit the internal Sync Right Ast node";
       (* Printf.printf "Value %s\n" (List.nth holder 0); *)
       let l1 = List.nth holder index in
       Printf.printf "Value of l1 %s\n" l1;
       let l2 = List.nth holder (index + 1) in
       Printf.printf "Value of l2 %s\n" l2;
       Sync
         ( LocId (l1, a)
         , LabelId ("R57", a)
         , LocId (l2, a)
         , copy_choreo_expr_right_sync e holder (index + 2) a
         , a )
     | _ ->
       print_endline "Goes in here";
       let l1 = List.nth holder index in
       let l2 = List.nth holder (index + 1) in
       Printf.printf "Value of l1 %s\n" l1;
       Printf.printf "Value of l2 %s\n" l2;
       Sync
         (LocId (l1, a), LabelId ("R67", a), LocId (l2, a), copy_choreo_expr e holder, a))
  | _ ->
    print_endline "This is being used";
    Unit a

and copy_choreo_expr_left_sync (x : 'a Choreo.expr) (holder : string list) (index : int) a
  : 'a Choreo.expr
  =
  print_endline "Inside here ie the left sync expr";
  match x with
  | Sync (_, _, _, e, a) ->
    print_endline "Hit the Sync Left Ast node";
    (match e with
     | Sync (_, _, _, _, _) ->
       print_endline "Hit the internal Sync Left Ast node";
       (* Printf.printf "Value %s\n" (List.nth holder 0); *)
       let l1 = List.nth holder index in
       Printf.printf "Value of l1 %s\n" l1;
       let l2 = List.nth holder (index + 1) in
       Printf.printf "Value of l2 %s\n" l2;
       Sync
         ( LocId (l1, a)
         , LabelId ("L57", a)
         , LocId (l2, a)
         , copy_choreo_expr_left_sync e holder (index + 2) a
         , a )
     | _ ->
       print_endline "Goes in here";
       let l1 = List.nth holder index in
       let l2 = List.nth holder (index + 1) in
       Printf.printf "Value of l1 %s\n" l1;
       Printf.printf "Value of l2 %s\n" l2;
       Sync
         (LocId (l1, a), LabelId ("L67", a), LocId (l2, a), copy_choreo_expr e holder, a))
  | _ ->
    print_endline "This is being used";
    Unit a

and copy_choreo_typ (x : 'a Choreo.typ) (holder : string list) : 'a Choreo.typ =
  match x with
  | TUnit a ->
    (* print_endline "Hit the Choreo typ's TUnit AST node"; *)
    TUnit a
  | TLoc (t1, t2, a) ->
    (* print_endline "Hit the Choreo typ's TLoc AST node"; *)
    TLoc (copy_loc_id t1 holder, copy_local_typ t2 holder, a)
  | TMap (t1, t2, a) ->
    (* print_endline "Hit the Choreo typ's TMap AST node"; *)
    TMap (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)
  | TProd (t1, t2, a) ->
    (* print_endline "Hit the Choreo typ's TProd AST node"; *)
    TProd (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)
  | TSum (t1, t2, a) ->
    (* print_endline "Hit the Choreo typ's TSum AST node"; *)
    TSum (copy_choreo_typ t1 holder, copy_choreo_typ t2 holder, a)

and copy_local_typ (x : 'a Local.typ) (holder : string list) =
  match x with
  | TUnit a ->
    (* print_endline "Hit the Local typ's TUnit AST node"; *)
    TUnit a
  | TInt a ->
    (* print_endline "Hit the Local typ's TInt AST node"; *)
    TInt a
  | TString a ->
    (* print_endline "Hit the Local typ's TString AST node"; *)
    TString a
  | TBool a ->
    (* print_endline "Hit the Local typ's TBool AST node"; *)
    TBool a
  | TProd (t1, t2, a) ->
    (* print_endline "Hit the Local typ's TProd AST node"; *)
    TProd (copy_local_typ t1 holder, copy_local_typ t2 holder, a)
  | TSum (t1, t2, a) ->
    (* print_endline "Hit the Local typ's TSum AST node"; *)
    TSum (copy_local_typ t1 holder, copy_local_typ t2 holder, a)

and copy_loc_id (x : 'a Local.loc_id) (_holder : string list) : 'a Local.loc_id =
  match x with
  | LocId (id, a) ->
    (* print_endline "Hit the Loc Id AST node"; *)
    LocId (id, a)

and _get_copy_loc_id (x : 'a Local.loc_id) : string =
  match x with
  | LocId (id, _) ->
    (* print_endline "Hit the Loc Id AST node"; *)
    id

and copy_loc_typ_id (x : 'a Local.typ_id) (_holder : string list) : 'a Local.typ_id =
  match x with
  | TypId (s, a) ->
    (* print_endline "Hit the Typ ID Ast node"; *)
    TypId (s, a)

and copy_sync_label_id (x : 'a Local.sync_label) : 'a Local.sync_label =
  match x with
  | LabelId (s, a) ->
    (* print_endline "Hit the Sync Label Id AST node"; *)
    LabelId (s, a)

and copy_choreo_case
      (x : ('a Choreo.pattern * 'a Choreo.expr) list)
      (holder : string list)
  : ('a Choreo.pattern * 'a Choreo.expr) list
  =
  match x with
  | [] ->
    (* print_endline "Done with the list of cases"; *)
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
    (* print_endline "Hit a Default AST node"; *)
    Default a
  | Var (v, a) ->
    (* print_endline "Hit a Var AST node"; *)
    Var (copy_loc_var_id v, a)
  | Pair (p1, p2, a) ->
    (* print_endline "Hit a Pair AST node"; *)
    Pair (copy_choreo_pattern p1 holder, copy_choreo_pattern p2 holder, a)
  | LocPat (l1, l2, a) ->
    (* print_endline "Hit a LocalPattern AST node"; *)
    LocPat (copy_loc_id l1 holder, copy_loc_pattern l2 holder, a)
  | Left (p, a) ->
    (* print_endline "Hit a Left AST node"; *)
    Left (copy_choreo_pattern p holder, a)
  | Right (p, a) ->
    (* print_endline "Hit a Right AST node"; *)
    Right (copy_choreo_pattern p holder, a)

and copy_get_pattern_list (p : 'a Choreo.pattern list) (holder : string list) =
  match p with
  | [] ->
    (* print_endline "Done with the pattern list"; *)
    []
  | head :: tail -> copy_choreo_pattern head holder :: copy_get_pattern_list tail holder
;;
