let rec jsonify_stmt_block (stmts: Choreo.stmt_block) =
  `Assoc [ ("stmt_block", `List (List.map jsonify_stmt stmts)) ]

and jsonify_stmt = function
  | Decl (p, t) ->
      `Assoc
        [
          ( "Decl",
            `Assoc [ ("choreo_pattern", jsonify_choreo_pattern p); ("choreo_type", jsonify_choreo_type t) ]
          );
        ]
  | Assign (p, e) ->
      `Assoc
        [
          ( "Assign",
            `Assoc [ ("choreo_pattern", jsonify_choreo_pattern p); ("choreo_expr", jsonify_choreo_expr e) ]
          );
        ]
  | TypeDecl (TypId id, t) ->
      `Assoc
        [
          ( "TypeDecl",
            `Assoc [ ("id", `String id); ("choreo_type", jsonify_choreo_type t) ]
          );
        ]

and jsonify_choreo_expr = function
  | Unit -> `String "Unit"
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", jsonify_choreo_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", jsonify_choreo_expr e) ]
  | Left e ->
      `Assoc [ ("Left", jsonify_choreo_expr e) ]
  | Right e ->
      `Assoc [ ("Right", jsonify_choreo_expr e) ]
  | LocExpr (LocId loc, e) ->
      `Assoc
        [
          ( "LocExpr",
            `Assoc
              [ ("loc", `String loc); ("local_expr", jsonify_local_expr e) ]
          );
        ]
  | Send (e, LocId loc) ->
      `Assoc
        [
          ( "Send",
            `Assoc
              [ ("choreo_expr", jsonify_choreo_expr e); ("loc", `String loc) ]
          );
        ]
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
      `Assoc
        [
          ( "Sync",
            `Assoc
              [
                ("loc1", `String loc1);
                ("label", `String label);
                ("loc2", `String loc2);
                ("choreo_expr", jsonify_choreo_expr e);
              ] );
        ]
  | If (e1, e2, e3) ->
      `Assoc
        [
          ( "If",
            `Assoc
              [
                ("condition", jsonify_choreo_expr e1);
                ("then", jsonify_choreo_expr e2);
                ("else", jsonify_choreo_expr e3);
              ] );
        ]
  | Let (stmts, e) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("stmt_block", `List (List.map jsonify_stmt stmts));
                ("choreo_expr", jsonify_choreo_expr e);
              ] );
        ]
  | FunDef (p, e) ->
      `Assoc
        [
          ( "FunDef",
            `Assoc [ ("pattern", jsonify_choreo_pattern p); ("choreo_expr", jsonify_choreo_expr e) ]
          );
        ]
  | FunApp (e1, e2) ->
      `Assoc
        [
          ( "FunApp",
            `Assoc
              [ ("fun", jsonify_choreo_expr e1); ("arg", jsonify_choreo_expr e2) ] );
        ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ jsonify_choreo_expr e1; jsonify_choreo_expr e2 ]) ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
            `Assoc
              [
                ("choreo_expr", jsonify_choreo_expr e);
                ("cases", `List (List.map jsonify_choreo_case cases));
              ] );
        ]

and jsonify_local_expr = function
  | Unit -> `String "Unit"
  | Val (`Int _ | `String _ | `Bool _ as v) ->
      `Assoc [ ("Val", v) ]
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", jsonify_local_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", jsonify_local_expr e) ]
  | Left e ->
      `Assoc [ ("Left", jsonify_local_expr e) ]
  | Right e ->
      `Assoc [ ("Right", jsonify_local_expr e) ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ jsonify_local_expr e1; jsonify_local_expr e2 ]) ]
  | BinOp (e1, op, e2) ->
      `Assoc
        [
          ( "BinOp",
            `Assoc
              [
                ("choreo_e1", jsonify_local_expr e1);
                ("op", jsonify_bin_op op);
                ("choreo_e2", jsonify_local_expr e2);
              ] );
        ]
  | Let (VarId id, e1, e2) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("id", `String id);
                ("binding", jsonify_local_expr e1);
                ("body", jsonify_local_expr e2);
              ] );
        ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
            `Assoc
              [
                ("local_expr", jsonify_local_expr e);
                ("cases", `List (List.map jsonify_local_case cases));
              ] );
        ]

and jsonify_choreo_case (p, e) =
  `Assoc [ ("choreo_pattern", jsonify_choreo_pattern p); ("choreo_expr", jsonify_choreo_expr e) ]

and jsonify_local_case (p, e) =
  `Assoc [ ("local_patt", jsonify_local_pattern p); ("local_expr", jsonify_local_expr e) ]

and jsonify_choreo_pattern = function
  | Default -> `String "Default"
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Left p ->
      `Assoc [ ("Left", jsonify_choreo_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", jsonify_choreo_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc [ ("Pair", `List [ jsonify_choreo_pattern p1; jsonify_choreo_pattern p2 ]) ]
  | LocPatt (LocId loc, p) ->
      `Assoc
        [
          ( "LocPatt",
            `Assoc
              [
                ("loc", `String loc); ("local_patt", jsonify_local_pattern p);
              ] );
        ]

and jsonify_local_pattern = function
  | Default -> `String "Default"
  | Val (`Int _ | `String _ | `Bool _ as v)  ->
      `Assoc [ ("Val", v) ]
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Left p ->
      `Assoc [ ("Left", jsonify_local_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", jsonify_local_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc
        [ ("Pair", `List [ jsonify_local_pattern p1; jsonify_local_pattern p2 ]) ]

and jsonify_choreo_type = function
  | TUnit -> `String "TUnit"
  | TLoc (LocId loc, t) ->
      `Assoc
        [
          ( "TLoc",
            `Assoc
              [ ("loc", `String loc); ("local_type", jsonify_local_type t) ]
          );
        ]
  | TMap (t1, t2) ->
      `Assoc [ ("TMap", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ]) ]
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ jsonify_choreo_type t1; jsonify_choreo_type t2 ]) ]

and jsonify_local_type = function
  | TUnit -> `String "TUnit"
  | TInt -> `String "TInt"
  | TString -> `String "TString"
  | TBool -> `String "TBool"
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ jsonify_local_type t1; jsonify_local_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ jsonify_local_type t1; jsonify_local_type t2 ]) ]

and jsonify_bin_op = function
  | Plus  -> `String "Plus"
  | Minus -> `String "Minus"
  | Times -> `String "Times"
  | Div   -> `String "Div"
  | And   -> `String "And"
  | Or    -> `String "Or"
  | Eq    -> `String "Eq"
  | Neq   -> `String "Neq"
  | Lt    -> `String "Lt"
  | Leq   -> `String "Leq"
  | Gt    -> `String "Gt"
  | Geq   -> `String "Geq"
