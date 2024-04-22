let rec dump_stmt_block (stmts: Choreo.stmt_block) =
  `Assoc [ ("stmt_block", `List (List.map dump_stmt stmts)) ]

and dump_stmt = function
  | Decl (p, t) ->
      `Assoc
        [
          ( "Decl",
            `Assoc [ ("choreo_pattern", dump_choreo_pattern p); ("choreo_type", dump_choreo_type t) ]
          );
        ]
  | Assign (p, e) ->
      `Assoc
        [
          ( "Assign",
            `Assoc [ ("choreo_pattern", dump_choreo_pattern p); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | TypeDecl (TypId id, t) ->
      `Assoc
        [
          ( "TypeDecl",
            `Assoc [ ("id", `String id); ("choreo_type", dump_choreo_type t) ]
          );
        ]

and dump_choreo_expr = function
  | Unit -> `String "Unit"
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", dump_choreo_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", dump_choreo_expr e) ]
  | Left e ->
      `Assoc [ ("Left", dump_choreo_expr e) ]
  | Right e ->
      `Assoc [ ("Right", dump_choreo_expr e) ]
  | LocExpr (LocId loc, e) ->
      `Assoc
        [
          ( "LocExpr",
            `Assoc
              [ ("loc", `String loc); ("local_expr", dump_local_expr e) ]
          );
        ]
  | Send (e, LocId loc1, LocId loc2) ->
      `Assoc
        [
          ( "Send",
            `Assoc
              [ ("choreo_expr", dump_choreo_expr e); ("from", `String loc1); ("to", `String loc2) ]
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
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | If (e1, e2, e3) ->
      `Assoc
        [
          ( "If",
            `Assoc
              [
                ("condition", dump_choreo_expr e1);
                ("then", dump_choreo_expr e2);
                ("else", dump_choreo_expr e3);
              ] );
        ]
  | Let (stmts, e) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("stmt_block", `List (List.map dump_stmt stmts));
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | FunDef (p, e) ->
      `Assoc
        [
          ( "FunDef",
            `Assoc [ ("pattern", dump_choreo_pattern p); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | FunApp (e1, e2) ->
      `Assoc
        [
          ( "FunApp",
            `Assoc
              [ ("fun", dump_choreo_expr e1); ("arg", dump_choreo_expr e2) ] );
        ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ dump_choreo_expr e1; dump_choreo_expr e2 ]) ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
            `Assoc
              [
                ("choreo_expr", dump_choreo_expr e);
                ("cases", `List (List.map dump_choreo_case cases));
              ] );
        ]

and dump_local_expr = function
  | Unit -> `String "Unit"
  | Val (`Int _ | `String _ | `Bool _ as v) ->
      `Assoc [ ("Val", v) ]
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Fst e ->
      `Assoc [ ("Fst", dump_local_expr e) ]
  | Snd e ->
      `Assoc [ ("Snd", dump_local_expr e) ]
  | Left e ->
      `Assoc [ ("Left", dump_local_expr e) ]
  | Right e ->
      `Assoc [ ("Right", dump_local_expr e) ]
  | Pair (e1, e2) ->
      `Assoc [ ("Pair", `List [ dump_local_expr e1; dump_local_expr e2 ]) ]
  | BinOp (e1, op, e2) ->
      `Assoc
        [
          ( "BinOp",
            `Assoc
              [
                ("choreo_e1", dump_local_expr e1);
                ("op", dump_bin_op op);
                ("choreo_e2", dump_local_expr e2);
              ] );
        ]
  | Let (VarId id, e1, e2) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("id", `String id);
                ("binding", dump_local_expr e1);
                ("body", dump_local_expr e2);
              ] );
        ]
  | Match (e, cases) ->
      `Assoc
        [
          ( "Match",
            `Assoc
              [
                ("local_expr", dump_local_expr e);
                ("cases", `List (List.map dump_local_case cases));
              ] );
        ]

and dump_choreo_case (p, e) =
  `Assoc [ ("choreo_pattern", dump_choreo_pattern p); ("choreo_expr", dump_choreo_expr e) ]

and dump_local_case (p, e) =
  `Assoc [ ("local_patt", dump_local_pattern p); ("local_expr", dump_local_expr e) ]

and dump_choreo_pattern = function
  | Default -> `String "Default"
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Left p ->
      `Assoc [ ("Left", dump_choreo_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", dump_choreo_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc [ ("Pair", `List [ dump_choreo_pattern p1; dump_choreo_pattern p2 ]) ]
  | LocPatt (LocId loc, p) ->
      `Assoc
        [
          ( "LocPatt",
            `Assoc
              [
                ("loc", `String loc); ("local_patt", dump_local_pattern p);
              ] );
        ]

and dump_local_pattern = function
  | Default -> `String "Default"
  | Val (`Int _ | `String _ | `Bool _ as v)  ->
      `Assoc [ ("Val", v) ]
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Left p ->
      `Assoc [ ("Left", dump_local_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", dump_local_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc
        [ ("Pair", `List [ dump_local_pattern p1; dump_local_pattern p2 ]) ]

and dump_choreo_type = function
  | TUnit -> `String "TUnit"
  | TLoc (LocId loc, t) ->
      `Assoc
        [
          ( "TLoc",
            `Assoc
              [ ("loc", `String loc); ("local_type", dump_local_type t) ]
          );
        ]
  | TMap (t1, t2) ->
      `Assoc [ ("TMap", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]

and dump_local_type = function
  | TUnit -> `String "TUnit"
  | TInt -> `String "TInt"
  | TString -> `String "TString"
  | TBool -> `String "TBool"
  | TProd (t1, t2) ->
      `Assoc [ ("TProd", `List [ dump_local_type t1; dump_local_type t2 ]) ]
  | TSum (t1, t2) ->
      `Assoc [ ("TSum", `List [ dump_local_type t1; dump_local_type t2 ]) ]

and dump_bin_op = function
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

let dump_choreo_ast (Choreo.Prog prog) = dump_stmt_block prog |> Yojson.Basic.pretty_to_string