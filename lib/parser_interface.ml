open Ast
open Lexer
open Lexing
open Yojson.Basic

let pos_string pos =
  let l = string_of_int pos.pos_lnum
  and c = string_of_int (pos.pos_cnum - pos.pos_bol + 1) in
  "line " ^ l ^ ", column " ^ c

let parse_program lexbuf =
  try Parser.program Lexer.token lexbuf with
  | SyntaxError msg ->
      raise (Failure ("Syntax error: " ^ msg ^ " at " ^ pos_string lexbuf.lex_start_p))
  | Parser.Error ->
      raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_start_p))

(* Printing utilities *)
let rec dump_ast prog = dump_program prog |> pretty_to_string

and dump_program (Prog statements) =
  `Assoc [ ("decl_block", `List (List.map dump_stmt statements)) ]

and dump_stmt = function
  | VarDecl (VarId id, t) ->
      `Assoc
        [
          ("node", `String "VarDecl");
          ("id", `String id);
          ("choreo_t", dump_choreo_type t);
        ]
  | FunDecl (FunId id, t1, t2) ->
      `Assoc
        [
          ("node", `String "FunDecl");
          ("id", `String id);
          ("choreo_t1", dump_choreo_type t1);
          ("choreo_t2", dump_choreo_type t2);
        ]
  | LocVarDecl (LocId loc1, VarId var, LocId loc2, t) ->
      `Assoc
        [
          ("node", `String "LocVarDecl");
          ("loc1", `String loc1);
          ("var", `String var);
          ("loc2", `String loc2);
          ("local_t", dump_local_type t);
        ]
  | TypeDecl (TypeId id, t) ->
      `Assoc
        [
          ("node", `String "TypeDecl");
          ("id", `String id);
          ("choreo_t", dump_choreo_type t);
        ]
  | VarAssign (VarId id, e) ->
      `Assoc
        [
          ("node", `String "VarAssign");
          ("id", `String id);
          ("C", dump_choreo_expr e);
        ]
  | FunAssign (FunId id, ps, e) ->
      `Assoc
        [
          ("node", `String "FunAssign");
          ("id", `String id);
          ("Ps", `List (List.map dump_pattern ps));
          ("C", dump_choreo_expr e);
        ]
  | LocVarAssign (LocId loc, VarId var, e) ->
      `Assoc
        [
          ("node", `String "LocVarAssign");
          ("loc", `String loc);
          ("var", `String var);
          ("C", dump_choreo_expr e);
        ]

and dump_choreo_expr = function
  | Unit -> `Assoc [ ("node", `String "Unit") ]
  | Var (VarId id) -> `Assoc [ ("node", `String "Var"); ("id", `String id) ]
  | Fst e -> `Assoc [ ("node", `String "Fst"); ("C", dump_choreo_expr e) ]
  | Snd e -> `Assoc [ ("node", `String "Snd"); ("C", dump_choreo_expr e) ]
  | Left e -> `Assoc [ ("node", `String "Left"); ("C", dump_choreo_expr e) ]
  | Right e -> `Assoc [ ("node", `String "Right"); ("C", dump_choreo_expr e) ]
  | LocExpr (LocId loc, e) ->
      `Assoc
        [
          ("node", `String "LocExpr");
          ("loc", `String loc);
          ("e", dump_local_expr e);
        ]
  | LocSend (LocId loc1, e, LocId loc2, VarId var, e') ->
      `Assoc
        [
          ("node", `String "LocSend");
          ("loc1", `String loc1);
          ("e", dump_local_expr e);
          ("loc2", `String loc2);
          ("var", `String var);
          ("C", dump_choreo_expr e');
        ]
  | Send (e, LocId loc) ->
      `Assoc
        [
          ("node", `String "Send");
          ("C", dump_choreo_expr e);
          ("loc", `String loc);
        ]
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
      `Assoc
        [
          ("node", `String "Sync");
          ("loc1", `String loc1);
          ("label", `String label);
          ("loc2", `String loc2);
          ("C", dump_choreo_expr e);
        ]
  | If (e1, e2, e3) ->
      `Assoc
        [
          ("node", `String "If");
          ("C1", dump_choreo_expr e1);
          ("C2", dump_choreo_expr e2);
          ("C3", dump_choreo_expr e3);
        ]
  | Let (decl_block, e) ->
      `Assoc
        [
          ("node", `String "Let");
          ("decl_block", `List (List.map dump_stmt decl_block));
          ("C", dump_choreo_expr e);
        ]
  | FunDef (FunId id, e) ->
    `Assoc
      [
        ("node", `String "FunDef");
        ("id", `String id);
        ("C", dump_choreo_expr e);
      ]
  | FunApp (e1, e2) ->
      `Assoc
        [
          ("node", `String "FunApp");
          ("C1", dump_choreo_expr e1);
          ("C2", dump_choreo_expr e2);
        ]
  | Pair (e1, e2) ->
      `Assoc
        [
          ("node", `String "Pair");
          ("C1", dump_choreo_expr e1);
          ("C2", dump_choreo_expr e2);
        ]
  | Match (e, cases) ->
    `Assoc
      [
        ("node", `String "Match");
        ("C", dump_choreo_expr e);
        ("cases", `List (List.map dump_case cases));
      ]

and dump_local_expr = function
  | Unit -> `Assoc [ ("node", `String "Unit") ]
  | Int i -> `Assoc [ ("node", `String "Int"); ("value", `Int i) ]
  | Val v -> `Assoc [ ("node", `String "Val"); ("value", dump_value v) ]
  | Var (VarId id) -> `Assoc [ ("node", `String "Var"); ("id", `String id) ]
  | Fst e -> `Assoc [ ("node", `String "Fst"); ("e", dump_local_expr e) ]
  | Snd e -> `Assoc [ ("node", `String "Snd"); ("e", dump_local_expr e) ]
  | Left e -> `Assoc [ ("node", `String "Left"); ("e", dump_local_expr e) ]
  | Right e -> `Assoc [ ("node", `String "Right"); ("e", dump_local_expr e) ]
  | BinOp (e1, op, e2) ->
      `Assoc
        [
          ("node", `String "BinOp");
          ("C1", dump_local_expr e1);
          ("op", dump_bin_op op);
          ("C2", dump_local_expr e2);
        ]
  | Let (VarId id, e1, e2) ->
      `Assoc
        [
          ("node", `String "Let");
          ("id", `String id);
          ("C1", dump_local_expr e1);
          ("C2", dump_local_expr e2);
        ]
  | Pair (e1, e2) ->
      `Assoc
        [
          ("node", `String "Pair");
          ("C1", dump_local_expr e1);
          ("C2", dump_local_expr e2);
        ]
  | Match (e, cases) ->
    `Assoc
      [
        ("node", `String "Match");
        ("e", dump_local_expr e);
        ("cases", `List (List.map dump_local_case cases));
      ]

and dump_case (p, e) =
  `Assoc [ ("P", dump_pattern p); ("C", dump_choreo_expr e) ]

and dump_local_case (p, e) =
  `Assoc [ ("p", dump_local_pattern p); ("e", dump_local_expr e) ]

and dump_pattern = function
  | Default -> `Assoc [ ("node", `String "Default") ]
  | Var (VarId id) -> `Assoc [ ("node", `String "Var"); ("id", `String id) ]
  | Left p -> `Assoc [ ("node", `String "Left"); ("p", dump_pattern p) ]
  | Right p -> `Assoc [ ("node", `String "Right"); ("p", dump_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc
        [
          ("node", `String "Pair");
          ("P1", dump_pattern p1);
          ("P2", dump_pattern p2);
        ]
  | LocPatt (LocId loc, p) ->
      `Assoc
        [
          ("node", `String "LocPatt");
          ("loc", `String loc);
          ("p", dump_local_pattern p);
        ]

and dump_local_pattern = function
  | Default -> `Assoc [ ("node", `String "Default") ]
  | Val v -> `Assoc [ ("node", `String "Val"); ("value", dump_value v) ]
  | Var (VarId id) -> `Assoc [ ("node", `String "Var"); ("id", `String id) ]
  | Left p -> `Assoc [ ("node", `String "Left"); ("p", dump_local_pattern p) ]
  | Right p -> `Assoc [ ("node", `String "Right"); ("p", dump_local_pattern p) ]
  | Pair (p1, p2) ->
      `Assoc
        [
          ("node", `String "Pair");
          ("p1", dump_local_pattern p1);
          ("p2", dump_local_pattern p2);
        ]

and dump_choreo_type = function
  | TUnit -> `Assoc [ ("node", `String "TUnit") ]
  | TLoc (LocId loc, t) ->
      `Assoc
        [
          ("node", `String "TLoc");
          ("loc", `String loc);
          ("local_t", dump_local_type t);
        ]
  | TSend (t1, t2) ->
      `Assoc
        [
          ("node", `String "TSend");
          ("choreo_t1", dump_choreo_type t1);
          ("choreo_t2", dump_choreo_type t2);
        ]
  | TProd (t1, t2) ->
      `Assoc
        [
          ("node", `String "TProd");
          ("choreo_t1", dump_choreo_type t1);
          ("choreo_t2", dump_choreo_type t2);
        ]
  | TSum (t1, t2) ->
      `Assoc
        [
          ("node", `String "TSum");
          ("choreo_t1", dump_choreo_type t1);
          ("choreo_t2", dump_choreo_type t2);
        ]

and dump_local_type = function
  | TUnit -> `Assoc [ ("node", `String "TUnit") ]
  | TInt -> `Assoc [ ("node", `String "TInt") ]
  | TString -> `Assoc [ ("node", `String "TString") ]
  | TBool -> `Assoc [ ("node", `String "TBool") ]
  | TProd (t1, t2) ->
      `Assoc
        [
          ("node", `String "TProd");
          ("local_t1", dump_local_type t1);
          ("local_t2", dump_local_type t2);
        ]
  | TSum (t1, t2) ->
      `Assoc
        [
          ("node", `String "TSum");
          ("local_t1", dump_local_type t1);
          ("local_t2", dump_local_type t2);
        ]

and dump_value = function
  | Int i -> `Assoc [ ("node", `String "Int"); ("value", `Int i) ]
  | String s -> `Assoc [ ("node", `String "String"); ("value", `String s) ]
  | Bool b -> `Assoc [ ("node", `String "Bool"); ("value", `Bool b) ]

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
