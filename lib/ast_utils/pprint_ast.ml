open Format

(* ============================== Local ============================== *)

(* unit
   int
   string
   bool
   type * type
   type + type
*)
let rec pprint_local_type ppf (typ : Ast.Local.typ) =
  match typ with
  | TUnit -> fprintf ppf "@[<h>unit@]"
  | TInt -> fprintf ppf "@[<h>int@]"
  | TString -> fprintf ppf "@[<h>string@]"
  | TBool -> fprintf ppf "@[<h>bool@]"
  | TProd (t1, t2) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_local_type t1 pprint_local_type t2
  | TSum (t1, t2) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_local_type t1 pprint_local_type t2
;;

(* _
   value
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
let rec pprint_local_pattern ppf (pat : Ast.Local.pattern) =
  match pat with
  | Default -> fprintf ppf "@[<h>_@]"
  | Val v ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf -> function
        | Ast.Local.Int i -> fprintf ppf "%d" i
        | String s -> fprintf ppf "%s" s
        | Bool b -> fprintf ppf "%b" b)
      v
  | Var (VarId id) -> fprintf ppf "@[<h>%s@]" id
  | Pair (p1, p2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_local_pattern p1 pprint_local_pattern p2
  | Left p -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_local_pattern p
  | Right p -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_local_pattern p
;;

(* ()
   value
   id
   (expr1) bin_op (expr2)
   let var_id := (expr1) in (expr2)
   ((expr1), (expr2))
   fst (expr)
   snd (expr)
   left (expr)
   right (expr)
   match (expr) with
   | case1
   | case2
   | ...
*)
let rec pprint_local_expr ppf (expr : Ast.Local.expr) =
  match expr with
  | Unit -> fprintf ppf "@[<h>()@]"
  | Val v ->
    fprintf
      ppf
      "@[<h>%a@]"
      (fun ppf -> function
        | Ast.Local.Int i -> fprintf ppf "%d" i
        | String s -> fprintf ppf "\"%s\"" s
        | Bool b -> fprintf ppf "%b" b)
      v
  | Var (VarId id) -> fprintf ppf "@[<h>%s@]" id
  | UnOp (op, e) ->
    fprintf
      ppf
      "@[<h>%s(%a)@]"
      (match op with
       | Not -> "not "
       | Neg -> "-")
      pprint_local_expr
      e
  | BinOp (e1, op, e2) ->
    fprintf
      ppf
      "@[<hv2>(%a) %s@ (%a)@]"
      pprint_local_expr
      e1
      (match op with
       | Plus -> "+"
       | Minus -> "-"
       | Times -> "*"
       | Div -> "/"
       | And -> "&&"
       | Or -> "||"
       | Eq -> "="
       | Neq -> "!="
       | Lt -> "<"
       | Leq -> "<="
       | Gt -> ">"
       | Geq -> ">=")
      pprint_local_expr
      e2
  | Let (VarId id, e1, e2) ->
    fprintf
      ppf
      "@[<hv2>let %s :=@ (%a)@;<1 -2>in@ (%a)@]"
      id
      pprint_local_expr
      e1
      pprint_local_expr
      e2
  | Pair (e1, e2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_local_expr e1 pprint_local_expr e2
  | Fst e -> fprintf ppf "@[<hv2>fst (%a)@]" pprint_local_expr e
  | Snd e -> fprintf ppf "@[<hv2>snd (%a)@]" pprint_local_expr e
  | Left e -> fprintf ppf "@[<hv2>left (%a)@]" pprint_local_expr e
  | Right e -> fprintf ppf "@[<hv2>right (%a)@]" pprint_local_expr e
  | Match (e, cases) ->
    let[@inline] pprint_local_case ppf (p, e) =
      (* pattern -> expr *)
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_local_expr e
    in
    fprintf
      ppf
      "@[<hv>match (%a)@ with@[<v2>@ | %a@]@]"
      pprint_local_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_local_case)
      cases
;;

(* ============================== Choreo ============================== *)

(*
   unit
   loc.(local_type)
   type -> type
   type * type
   type + type
*)
let rec pprint_choreo_type ppf (typ : Ast.Choreo.typ) =
  match typ with
  | TUnit -> fprintf ppf "@[<h>unit@]"
  | TLoc (LocId loc, t) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_type t
  | TMap (t1, t2) ->
    fprintf ppf "@[<h>(%a) -> (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TProd (t1, t2) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
  | TSum (t1, t2) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_choreo_type t1 pprint_choreo_type t2
;;

(* _
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
let rec pprint_choreo_pattern ppf (pat : Ast.Choreo.pattern) =
  match pat with
  | Default -> fprintf ppf "@[<h>_@]"
  | Var (VarId id) -> fprintf ppf "@[<h>%s@]" id
  | LocPatt (LocId loc, p) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_pattern p
  | Pair (p1, p2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_choreo_pattern p1 pprint_choreo_pattern p2
  | Left p -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_choreo_pattern p
  | Right p -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_choreo_pattern p
;;

(* {
      stmt1
      stmt2
      stmt3
      ...
    }
*)
let[@specialise] rec pprint_choreo_stmt_block ppf (stmts : Ast.Choreo.stmt_block) =
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]" (pp_print_list pprint_choreo_stmt) stmts

(* (choreo_pattern) : choreo_type
   (choreo_pattern) = (choreo_expr)
   id : choreo_type
*)
and pprint_choreo_stmt ppf = function
  | Decl (p, t) ->
    fprintf ppf "@[<h>(%a) : %a;@]" pprint_choreo_pattern p pprint_choreo_type t
  | Assign (ps, e) ->
    fprintf
      ppf
      "@[<hv2>(%a) :=@ (%a);@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | TypeDecl (TypId id, t) -> fprintf ppf "@[<h>%s : %a@]" id pprint_choreo_type t

(* ()
    id
    fst (expr)
    snd (expr)
    left (expr)
    right (expr)
    loc.(local_expr)
    (expr) ~> loc
    loc1[label] ~> loc2; (expr)
    if (condition) then (expr1) else (expr2)
    let {stmt_block} in (expr)
    fun (pattern) -> (expr)
    (fun) (arg)
    (expr1), (expr2)
    match (expr) with
    | case1
    | case2
    | ...
*)
and pprint_choreo_expr ppf = function
  | Unit -> fprintf ppf "Unit"
  | Var (VarId id) -> fprintf ppf "@[<h>%s@]" id
  | Fst e -> fprintf ppf "@[<hv2>fst@ (%a)@]" pprint_choreo_expr e
  | Snd e -> fprintf ppf "@[<hv2>snd@ (%a)@]" pprint_choreo_expr e
  | Left e -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_choreo_expr e
  | Right e -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_choreo_expr e
  | LocExpr (LocId loc, e) -> fprintf ppf "@[<h>%s.(%a)@]" loc pprint_local_expr e
  | Send (LocId loc1, e, LocId loc2) ->
    fprintf ppf "@[<hv2>[%s] (%a)@ ~> %s@]" loc1 pprint_choreo_expr e loc2
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
    fprintf ppf "@[<hv>%s[%s] ~> %s;@ (%a)@]" loc1 label loc2 pprint_choreo_expr e
  | If (e1, e2, e3) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
      pprint_choreo_expr
      e1
      pprint_choreo_expr
      e2
      pprint_choreo_expr
      e3
  | Let (stmt_block, e) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ (%a)@]"
      pprint_choreo_stmt_block
      stmt_block
      pprint_choreo_expr
      e
  | FunDef (ps, e) ->
    fprintf
      ppf
      "@[<hv2>fun@ (%a) ->@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_choreo_pattern)
      ps
      pprint_choreo_expr
      e
  | FunApp (e1, e2) ->
    fprintf ppf "@[<hv>(%a)@ (%a)@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Pair (e1, e2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_choreo_expr e1 pprint_choreo_expr e2
  | Match (e, cases) ->
    let[@inline] pprint_choreo_case ppf (p, e) =
      (* pattern -> expr *)
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_choreo_pattern p pprint_choreo_expr e
    in
    fprintf
      ppf
      "@[<v>match (%a)@ with@ | %a@]"
      pprint_choreo_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_choreo_case)
      cases
;;

(* ============================== Net ============================== *)
(*
   unit
   loc.(local_type)
   type -> type
   type * type
   type + type
*)
let rec pprint_net_type ppf (typ : Ast.Net.typ) =
  match typ with
  | TUnit -> fprintf ppf "@[<h>unit@]"
  | TLoc t -> fprintf ppf "@[<h>%a@]" pprint_local_type t
  | TMap (t1, t2) ->
    fprintf ppf "@[<h>(%a) -> (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TProd (t1, t2) ->
    fprintf ppf "@[<h>(%a) * (%a)@]" pprint_net_type t1 pprint_net_type t2
  | TSum (t1, t2) ->
    fprintf ppf "@[<h>(%a) + (%a)@]" pprint_net_type t1 pprint_net_type t2
;;

let[@specialise] rec pprint_net_stmt_block ppf (stmts : Ast.Net.stmt_block) =
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]" (pp_print_list pprint_net_stmt) stmts

and pprint_net_stmt ppf = function
  | Decl (p, t) -> fprintf ppf "@[<h>(%a) : %a@]" pprint_local_pattern p pprint_net_type t
  | Assign (ps, e) ->
    fprintf
      ppf
      "@[<hv2>(%a) :=@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | TypeDecl (TypId id, t) -> fprintf ppf "@[<h>%s : %a@]" id pprint_net_type t

and pprint_net_expr ppf = function
  | Unit -> fprintf ppf "@[<h>()@]"
  | Var (VarId id) -> fprintf ppf "@[<h>%s@]" id
  | Ret e -> fprintf ppf "@[<hv2>ret@ (%a)@]" pprint_local_expr e
  | Fst e -> fprintf ppf "@[<hv2>fst@ (%a)@]" pprint_net_expr e
  | Snd e -> fprintf ppf "@[<hv2>snd@ (%a)@]" pprint_net_expr e
  | Left e -> fprintf ppf "@[<hv2>left@ (%a)@]" pprint_net_expr e
  | Right e -> fprintf ppf "@[<hv2>right@ (%a)@]" pprint_net_expr e
  | Send (e, LocId loc) -> fprintf ppf "@[<hv2>(%a) ~> %s@]" pprint_net_expr e loc
  | Recv (LocId loc) -> fprintf ppf "@[<~ %s@]" loc
  | If (e1, e2, e3) ->
    fprintf
      ppf
      "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
      pprint_net_expr
      e1
      pprint_net_expr
      e2
      pprint_net_expr
      e3
  | Let (stmt_block, e) ->
    fprintf
      ppf
      "@[<hv2>let@ %a@;<1 -2>in@ (%a)@]"
      pprint_net_stmt_block
      stmt_block
      pprint_net_expr
      e
  | FunDef (ps, e) ->
    fprintf
      ppf
      "@[<hv2>fun@ (%a) ->@ (%a)@]"
      (pp_print_list ~pp_sep:pp_print_space pprint_local_pattern)
      ps
      pprint_net_expr
      e
  | FunApp (e1, e2) ->
    fprintf ppf "@[<hv>(%a)@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Pair (e1, e2) ->
    fprintf ppf "@[<hv>(%a),@ (%a)@]" pprint_net_expr e1 pprint_net_expr e2
  | Match (e, cases) ->
    let[@inline] pprint_net_case ppf (p, e) =
      fprintf ppf "@[<hv2>%a ->@ %a@]" pprint_local_pattern p pprint_net_expr e
    in
    fprintf
      ppf
      "@[<hv>match (%a)@ with@[<v2>@ | %a@]@]"
      pprint_net_expr
      e
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_case)
      cases
  | ChooseFor (LabelId id, LocId locid, e) ->
    fprintf ppf "@[<hv>choose %s for %s;@ %a@]" id locid pprint_net_expr e
  | AllowChoice (LocId loc, choices) ->
    let[@inline] pprint_net_choice ppf (Ast.Local.LabelId id, e) =
      fprintf ppf "@[<hv2>%s ->@ %a@]" id pprint_net_expr e
    in
    fprintf
      ppf
      "@[<v>allow %s choice@ | %a@]"
      loc
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@ | ") pprint_net_choice)
      choices
;;
