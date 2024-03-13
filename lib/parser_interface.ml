open Local_ast
open Choreo_ast
open Lexer
open Lexing
open Yojson.Basic

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  "line " ^ l ^ ", column " ^ c

let parse_program lexbuf =
  try Parser.program Lexer.read lexbuf with
  | SyntaxError msg ->
      raise (Failure ("Syntax error: " ^ msg ^ " at " ^ pos_string lexbuf.lex_curr_p))
  | Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))

(* Printing utilities *)
let rec dump_ast prog = dump_program prog |> pretty_to_string

and dump_program (Prog statements) =
  `Assoc [ ("decl_block", `List (List.map dump_stmt statements)) ]

and dump_stmt = function
  (* | VarDecl (VarId id, t) ->
      `Assoc
        [
          ( "VarDecl",
            `Assoc [ ("id", `String id); ("choreo_type", dump_choreo_type t) ]
          );
        ] *)
  (* | FunDecl (VarId id, t1, t2) ->
      `Assoc
        [
          ( "FunDecl",
            `Assoc
              [
                ("id", `String id);
                ("choreo_t1", dump_choreo_type t1);
                ("choreo_t2", dump_choreo_type t2);
              ] );
        ] *)
  (* | LocVarDecl (LocId loc1, VarId var, LocId loc2, t) ->
      `Assoc
        [
          ( "LocVarDecl",
            `Assoc
              [
                ("loc1", `String loc1);
                ("var", `String var);
                ("loc2", `String loc2);
                ("local_type", dump_local_type t);
              ] );
        ] *)
  | VarDecl (p, t) ->
      `Assoc
        [
          ( "VarDecl",
            `Assoc [ ("pattern", dump_pattern p); ("choreo_type", dump_choreo_type t) ]
          );
        ]
  | TypeDecl (VarId id, t) ->
      `Assoc
        [
          ( "TypeDecl",
            `Assoc [ ("id", `String id); ("choreo_type", dump_choreo_type t) ]
          );
        ]
  | VarAssign (VarId id, e) ->
      `Assoc
        [
          ( "VarAssign",
            `Assoc [ ("var", `String id); ("choreo_expr", dump_choreo_expr e) ]
          );
        ]
  | FunAssign (VarId id, ps, e) ->
      `Assoc
        [
          ( "FunAssign",
            `Assoc
              [
                ("fun", `String id);
                ("patterns", `List (List.map dump_pattern ps));
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | LocVarAssign (LocId loc, VarId var, e) ->
      `Assoc
        [
          ( "LocVarAssign",
            `Assoc
              [
                ("loc", `String loc);
                ("var", `String var);
                ("choreo_expr", dump_choreo_expr e);
              ] );
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
  | Send (e, LocId loc) ->
      `Assoc
        [
          ( "Send",
            `Assoc
              [ ("choreo_expr", dump_choreo_expr e); ("loc", `String loc) ]
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
  | Let (decl_block, e) ->
      `Assoc
        [
          ( "Let",
            `Assoc
              [
                ("decl_block", `List (List.map dump_stmt decl_block));
                ("choreo_expr", dump_choreo_expr e);
              ] );
        ]
  | FunDef (VarId id, e) ->
      `Assoc
        [
          ( "FunDef",
            `Assoc [ ("id", `String id); ("choreo_expr", dump_choreo_expr e) ]
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
                ("cases", `List (List.map dump_case cases));
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

and dump_case (p, e) =
  `Assoc [ ("pattern", dump_pattern p); ("choreo_expr", dump_choreo_expr e) ]

and dump_local_case (p, e) =
  `Assoc [ ("local_patt", dump_local_pattern p); ("local_expr", dump_local_expr e) ]

and dump_pattern = function
  | Default -> `String "Default"
  | Var (VarId id) ->
      `Assoc [ ("Var", `String id) ]
  | Left p ->
      `Assoc [ ("Left", dump_pattern p) ]
  | Right p ->
      `Assoc [ ("Right", dump_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc [ ("Pair", `List [ dump_pattern p1; dump_pattern p2 ]) ]
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
  | TSend (t1, t2) ->
      `Assoc [ ("TSend", `List [ dump_choreo_type t1; dump_choreo_type t2 ]) ]
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
