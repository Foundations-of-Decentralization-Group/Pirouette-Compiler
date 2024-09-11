(* ============================== Local ============================== *)

let rec jsonify_local_expr (e : Ast.Local.expr) =
  match e with
  | Unit -> `String "Unit"
  | Val v ->
    `Assoc
      [ ( "Val"
        , match v with
          | Int i -> `Int i
          | String s -> `String s
          | Bool b -> `Bool b )
      ]
  | Var (VarId id) -> `Assoc [ "Var", `String id ]
  | Fst e -> `Assoc [ "Fst", jsonify_local_expr e ]
  | Snd e -> `Assoc [ "Snd", jsonify_local_expr e ]
  | Left e -> `Assoc [ "Left", jsonify_local_expr e ]
  | Right e -> `Assoc [ "Right", jsonify_local_expr e ]
  | Pair (e1, e2) ->
    `Assoc [ "Pair", `List [ jsonify_local_expr e1; jsonify_local_expr e2 ] ]
  | UnOp (op, e) ->
    `Assoc [ "UnOp", `Assoc [ "op", jsonify_un_op op; "choreo_e", jsonify_local_expr e ] ]
  | BinOp (e1, op, e2) ->
    `Assoc
      [ ( "BinOp"
        , `Assoc
            [ "choreo_e1", jsonify_local_expr e1
            ; "op", jsonify_bin_op op
            ; "choreo_e2", jsonify_local_expr e2
            ] )
      ]
  | Let (VarId id, e1, e2) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "id", `String id
            ; "binding", jsonify_local_expr e1
            ; "body", jsonify_local_expr e2
            ] )
      ]
  | Match (e, cases) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "local_expr", jsonify_local_expr e
            ; "cases", `List (List.map jsonify_local_case cases)
            ] )
      ]

and jsonify_local_case (p, e) =
  `Assoc [ "local_patt", jsonify_local_pattern p; "local_expr", jsonify_local_expr e ]

and jsonify_local_pattern = function
  | Default -> `String "Default"
  | Val v ->
    `Assoc
      [ ( "Val"
        , match v with
          | Int i -> `Int i
          | String s -> `String s
          | Bool b -> `Bool b )
      ]
  | Var (VarId id) -> `Assoc [ "Var", `String id ]
  | Left p -> `Assoc [ "Left", jsonify_local_pattern p ]
  | Right p -> `Assoc [ "Right", jsonify_local_pattern p ]
  | Pair (p1, p2) ->
    `Assoc [ "Pair", `List [ jsonify_local_pattern p1; jsonify_local_pattern p2 ] ]

and jsonify_local_type (t : Ast.Local.typ) =
  match t with
  | TUnit -> `String "TUnit"
  | TInt -> `String "TInt"
  | TString -> `String "TString"
  | TBool -> `String "TBool"
  | TProd (t1, t2) ->
    `Assoc [ "TProd", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]
  | TSum (t1, t2) ->
    `Assoc [ "TSum", `List [ jsonify_local_type t1; jsonify_local_type t2 ] ]

and jsonify_un_op = function
  | Not -> `String "Not"
  | Neg -> `String "Neg"

and jsonify_bin_op = function
  | Plus -> `String "Plus"
  | Minus -> `String "Minus"
  | Times -> `String "Times"
  | Div -> `String "Div"
  | And -> `String "And"
  | Or -> `String "Or"
  | Eq -> `String "Eq"
  | Neq -> `String "Neq"
  | Lt -> `String "Lt"
  | Leq -> `String "Leq"
  | Gt -> `String "Gt"
  | Geq -> `String "Geq"
;;

(* ============================== Choreo ============================== *)

let rec jsonify_choreo_stmt_block (stmts : Ast.Choreo.stmt_block) =
  `List (List.map jsonify_choreo_stmt stmts)

and jsonify_choreo_stmt = function
  | Decl (p, t) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "choreo_pattern", jsonify_choreo_pattern p
            ; "choreo_type", jsonify_choreo_type t
            ] )
      ]
  | Assign (ps, e) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "choreo_patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | TypeDecl (TypId id, t) ->
    `Assoc
      [ "TypeDecl", `Assoc [ "id", `String id; "choreo_type", jsonify_choreo_type t ] ]

and jsonify_choreo_expr = function
  | Unit -> `String "Unit"
  | Var (VarId id) -> `Assoc [ "Var", `String id ]
  | Fst e -> `Assoc [ "Fst", jsonify_choreo_expr e ]
  | Snd e -> `Assoc [ "Snd", jsonify_choreo_expr e ]
  | Left e -> `Assoc [ "Left", jsonify_choreo_expr e ]
  | Right e -> `Assoc [ "Right", jsonify_choreo_expr e ]
  | LocExpr (LocId loc, e) ->
    `Assoc
      [ "LocExpr", `Assoc [ "loc", `String loc; "local_expr", jsonify_local_expr e ] ]
  | Send (LocId loc1, e, LocId loc2) ->
    `Assoc
      [ ( "Send"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "from", `String loc1
            ; "to", `String loc2
            ] )
      ]
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
    `Assoc
      [ ( "Sync"
        , `Assoc
            [ "loc1", `String loc1
            ; "label", `String label
            ; "loc2", `String loc2
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | If (e1, e2, e3) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_choreo_expr e1
            ; "then", jsonify_choreo_expr e2
            ; "else", jsonify_choreo_expr e3
            ] )
      ]
  | Let (stmts, e) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_choreo_stmt stmts)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | FunDef (ps, e) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_choreo_pattern ps)
            ; "choreo_expr", jsonify_choreo_expr e
            ] )
      ]
  | FunApp (e1, e2) ->
    `Assoc
      [ "FunApp", `Assoc [ "fun", jsonify_choreo_expr e1; "arg", jsonify_choreo_expr e2 ]
      ]
  | Pair (e1, e2) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_expr e1; jsonify_choreo_expr e2 ] ]
  | Match (e, cases) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "choreo_expr", jsonify_choreo_expr e
            ; "cases", `List (List.map jsonify_choreo_case cases)
            ] )
      ]

and jsonify_choreo_case (p, e) =
  `Assoc
    [ "choreo_pattern", jsonify_choreo_pattern p; "choreo_expr", jsonify_choreo_expr e ]

and jsonify_choreo_pattern = function
  | Default -> `String "Default"
  | Var (VarId id) -> `Assoc [ "Var", `String id ]
  | Left p -> `Assoc [ "Left", jsonify_choreo_pattern p ]
  | Right p -> `Assoc [ "Right", jsonify_choreo_pattern p ]
  | Pair (p1, p2) ->
    `Assoc [ "Pair", `List [ jsonify_choreo_pattern p1; jsonify_choreo_pattern p2 ] ]
  | LocPatt (LocId loc, p) ->
    `Assoc
      [ "LocPatt", `Assoc [ "loc", `String loc; "local_patt", jsonify_local_pattern p ] ]

and jsonify_choreo_type = function
  | TUnit -> `String "TUnit"
  | TLoc (LocId loc, t) ->
    `Assoc [ "TLoc", `Assoc [ "loc", `String loc; "local_type", jsonify_local_type t ] ]
  | TMap (t1, t2) ->
    `Assoc [ "TMap", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TProd (t1, t2) ->
    `Assoc [ "TProd", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TSum (t1, t2) ->
    `Assoc [ "TSum", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ] ]
  | TAlias (TypId id, t) ->
    `Assoc [ "TAlias", `Assoc [ "id", `String id; "choreo_type", jsonify_choreo_type t ] ]
;;

(* ============================== Net ============================== *)

let rec jsonify_net_stmt_block (stmts : Ast.Net.stmt_block) =
  `List (List.map jsonify_net_stmt stmts)

and jsonify_net_stmt = function
  | Decl (p, t) ->
    `Assoc
      [ ( "Decl"
        , `Assoc
            [ "net_pattern", jsonify_local_pattern p; "net_type", jsonify_net_type t ] )
      ]
  | Assign (ps, e) ->
    `Assoc
      [ ( "Assign"
        , `Assoc
            [ "net_pattern", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | TypeDecl (TypId id, t) ->
    `Assoc [ "TypeDecl", `Assoc [ "id", `String id; "net_type", jsonify_net_type t ] ]

and jsonify_net_case (p, e) =
  `Assoc [ "net_pattern", jsonify_local_pattern p; "net_expr", jsonify_net_expr e ]

and jsonify_net_expr = function
  | Unit -> `String "Unit"
  | Var (VarId id) -> `Assoc [ "Var", `String id ]
  | Ret e -> `Assoc [ "Ret", jsonify_local_expr e ]
  | If (e1, e2, e3) ->
    `Assoc
      [ ( "If"
        , `Assoc
            [ "condition", jsonify_net_expr e1
            ; "then", jsonify_net_expr e2
            ; "else", jsonify_net_expr e3
            ] )
      ]
  | Let (stmts, e) ->
    `Assoc
      [ ( "Let"
        , `Assoc
            [ "stmt_block", `List (List.map jsonify_net_stmt stmts)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | Send (e, LocId loc) ->
    `Assoc [ "Send", `Assoc [ "net_expr", jsonify_net_expr e; "to", `String loc ] ]
  | Recv (LocId loc) -> `Assoc [ "Recv", `String loc ]
  | ChooseFor (LabelId label, LocId loc, e) ->
    `Assoc
      [ ( "ChooseFor"
        , `Assoc
            [ "label", `String label; "loc", `String loc; "net_expr", jsonify_net_expr e ]
        )
      ]
  | AllowChoice (LocId loc, choices) ->
    `Assoc
      [ ( "AllowChoice"
        , `Assoc
            [ "loc", `String loc
            ; ( "choices"
              , `List
                  (List.map
                     (fun (Ast.Local.LabelId label, e) ->
                       `Assoc [ "label", `String label; "net_expr", jsonify_net_expr e ])
                     choices) )
            ] )
      ]
  | FunDef (ps, e) ->
    `Assoc
      [ ( "FunDef"
        , `Assoc
            [ "patterns", `List (List.map jsonify_local_pattern ps)
            ; "net_expr", jsonify_net_expr e
            ] )
      ]
  | FunApp (e1, e2) ->
    `Assoc [ "FunApp", `Assoc [ "fun", jsonify_net_expr e1; "arg", jsonify_net_expr e2 ] ]
  | Pair (e1, e2) -> `Assoc [ "Pair", `List [ jsonify_net_expr e1; jsonify_net_expr e2 ] ]
  | Fst e -> `Assoc [ "Fst", jsonify_net_expr e ]
  | Snd e -> `Assoc [ "Snd", jsonify_net_expr e ]
  | Left e -> `Assoc [ "Left", jsonify_net_expr e ]
  | Right e -> `Assoc [ "Right", jsonify_net_expr e ]
  | Match (e, cases) ->
    `Assoc
      [ ( "Match"
        , `Assoc
            [ "net_expr", jsonify_net_expr e
            ; "cases", `List (List.map jsonify_net_case cases)
            ] )
      ]

and jsonify_net_type = function
  | TUnit -> `String "TUnit"
  | TLoc t -> `Assoc [ "TLoc", jsonify_local_type t ]
  | TMap (t1, t2) -> `Assoc [ "TMap", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | TProd (t1, t2) ->
    `Assoc [ "TProd", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
  | TSum (t1, t2) -> `Assoc [ "TSum", `List [ jsonify_net_type t1; jsonify_net_type t2 ] ]
;;
