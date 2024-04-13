open Format

(* {
     stmt1
     stmt2
     stmt3
     ...
   }
*)
let rec print_stmt_block ppf (stmts : Choreo.stmt_block) =
  fprintf ppf "@[<v>(@[<v1>@,%a@]@,)@]"
    (pp_print_list print_stmt) stmts

(* (choreo_pattern) : choreo_type
   (choreo_pattern) = (choreo_expr)
   id : choreo_type
*)
and print_stmt ppf = function
  | Decl (p, t) ->
      fprintf ppf "@[<h>(%a) : %a@];"
        print_choreo_pattern p
        print_choreo_type t
  | Assign (p, e) ->
      fprintf ppf "@[<hv2>(%a) :=@ (%a)@];"
        print_choreo_pattern p
        print_choreo_expr e
  | TypeDecl (TypId id, t) ->
      fprintf ppf "@[<h>%s : %a@]"
        id
        print_choreo_type t

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
and print_choreo_expr ppf = function
  | Unit -> fprintf ppf "Unit"
  | Var (VarId id) ->
      fprintf ppf "@[<h>%s@]" id
  | Fst e ->
      fprintf ppf "@[<hv2>fst@ (%a)@]" print_choreo_expr e
  | Snd e ->
      fprintf ppf "@[<hv2>snd@ (%a)@]" print_choreo_expr e
  | Left e ->
      fprintf ppf "@[<hv2>left@ (%a)@]" print_choreo_expr e
  | Right e ->
      fprintf ppf "@[<hv2>right@ (%a)@]" print_choreo_expr e
  | LocExpr (LocId loc, e) ->
      fprintf ppf "@[<h>%s.(%a)@]" loc print_local_expr e
  | Send (e, LocId loc) ->
      fprintf ppf "@[<hv2>(%a) ~>@ %s@]" print_choreo_expr e loc
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
      fprintf ppf "@[<hv2>%s[%s] ~>@ %s;@ (%a)@]"
        loc1 label loc2
        print_choreo_expr e
  | If (e1, e2, e3) ->
      fprintf ppf "@[<hv>if@;<1 2>(%a)@ then@;<1 2>(%a)@ else@;<1 2>(%a)@]"
        print_choreo_expr e1
        print_choreo_expr e2
        print_choreo_expr e3
  | Let (stmt_block, e) ->
      fprintf ppf "@[<hv>let@;<1 2>%a@ in (%a)@]"
        print_stmt_block stmt_block
        print_choreo_expr e
  | FunDef (p, e) -> 
      fprintf ppf "@[<hv2>fun@ (%a) ->@ (%a)@]"
        print_choreo_pattern p
        print_choreo_expr e
  | FunApp (e1, e2) ->
      fprintf ppf "@[<hv>(%a)@ (%a)@]"
        print_choreo_expr e1
        print_choreo_expr e2
  | Pair (e1, e2) ->
      fprintf ppf "@[<hv>(%a),@ (%a)@]"
        print_choreo_expr e1
        print_choreo_expr e2
  | Match (e, cases) ->
      fprintf ppf "@[<hv>match (%a)@ with@[<v2>@,| %a@]@]"
        print_choreo_expr e
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;<1 2>| ") print_choreo_case) cases

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
and print_local_expr ppf = function
  | Unit -> fprintf ppf "@[<h>()@]"
  | Val v ->
    fprintf ppf "@[<h>%a@]"
      (fun ppf -> function `Int i -> fprintf ppf "%d" i | `String s -> fprintf ppf "%s" s | `Bool b -> fprintf ppf "%b" b) v
  | Var (VarId id) ->
      fprintf ppf "@[<h>%s@]" id
  | BinOp (e1, op, e2) ->
      fprintf ppf "@[<hv2>(%a) %s@ (%a)@]"
      print_local_expr e1
      (match op with
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"
        | And -> "&&" | Or -> "||" | Eq -> "=" | Neq -> "!="
        | Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=")
      print_local_expr e2
  | Let (VarId id, e1, e2) ->
      fprintf ppf "@[<hv2>let %s :=@ (%a)@;<1 -2>in@ (%a)@]"
        id
        print_local_expr e1
        print_local_expr e2
  | Pair (e1, e2) ->
      fprintf ppf "@[<hv>(%a),@ (%a)@]"
        print_local_expr e1
        print_local_expr e2
  | Fst e ->
      fprintf ppf "@[<hv2>fst (%a)@]" print_local_expr e
  | Snd e ->
      fprintf ppf "@[<hv2>snd (%a)@]" print_local_expr e
  | Left e ->
      fprintf ppf  "@[<hv2>left (%a)@]" print_local_expr e
  | Right e ->
      fprintf ppf "@[<hv2>right (%a)@]" print_local_expr e
  | Match (e, cases) ->
      fprintf ppf "@[<hv>match (%a)@ with@[<v2>@,| %a@]@]"
        print_local_expr e
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;<1 2>| ") print_local_case) cases

(* pattern -> expr *)
and print_choreo_case ppf (p, e) =
  fprintf ppf "@[<hv2>%a ->@ %a@]"
    print_choreo_pattern p
    print_choreo_expr e

and print_local_case ppf (p, e) =
  fprintf ppf "@[<hv2>%a ->@ %a@]"
    print_local_pattern p
    print_local_expr e

(* _
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
and print_choreo_pattern ppf = function
  | Default ->
      fprintf ppf "@[<h>_@]"
  | Var (VarId id) ->
      fprintf ppf "@[<h>%s@]" id
  | LocPatt (LocId loc, p) ->
      fprintf ppf "@[<h>%s.(%a)@]" loc print_local_pattern p
  | Pair (p1, p2) ->
      fprintf ppf "@[<hv>(%a),@ (%a)@]"
        print_choreo_pattern p1
        print_choreo_pattern p2
  | Left p ->
      fprintf ppf "@[<hv2>left@ (%a)@]" print_choreo_pattern p
  | Right p ->
      fprintf ppf "@[<hv2>right@ (%a)@]" print_choreo_pattern p

(* _
   value
   id
   loc.(local_expr)
   (pattern1), (pattern2)
   loc.(local_pattern)
   left (pattern)
   right (pattern)
*)
and print_local_pattern ppf = function
  | Default ->
      fprintf ppf "@[<h>_@]"
  | Val v ->
    fprintf ppf "@[<h>%a@]"
      (fun ppf -> function `Int i -> fprintf ppf "%d" i | `String s -> fprintf ppf "%s" s | `Bool b -> fprintf ppf "%b" b) v
  | Var (VarId id) ->
      fprintf ppf "@[<h>%s@]" id
  | Pair (p1, p2) ->
      fprintf ppf "@[<hv>(%a),@ (%a)@]"
        print_local_pattern p1
        print_local_pattern p2
  | Left p ->
      fprintf ppf "@[<hv2>left@ (%a)@]" print_local_pattern p
  | Right p ->
      fprintf ppf "@[<hv2>right@ (%a)@]" print_local_pattern p

(*
   unit
   loc.(local_type)
   type -> type
   type * type
   type + type
*)
and print_choreo_type ppf = function
  | TUnit ->
      fprintf ppf "@[<h>unit@]"
  | TLoc (LocId loc, t) ->
      fprintf ppf "@[<h>%s.(%a)@]" loc print_local_type t
  | TMap (t1, t2) ->
      fprintf ppf "@[<h>(%a) -> (%a)@]"
        print_choreo_type t1 
        print_choreo_type t2
  | TProd (t1, t2) ->
      fprintf ppf "(@[<h>%a@]) * (@[<h>%a@])"
        print_choreo_type t1
        print_choreo_type t2
  | TSum (t1, t2) ->
      fprintf ppf "(@[<h>%a@]) + (@[<h>%a@])"
        print_choreo_type t1
        print_choreo_type t2

(* unit
   int
   string
   bool
   type * type
   type + type
*)
and print_local_type ppf = function
  | TUnit ->
      fprintf ppf "@[<h>unit@]"
  | TInt ->
      fprintf ppf "@[<h>int@]"
  | TString ->
      fprintf ppf "@[<h>string@]"
  | TBool ->
      fprintf ppf "@[<h>bool@]"
  | TProd (t1, t2) ->
      fprintf ppf "@[<h>(%a) * (%a)@]"
        print_local_type t1
        print_local_type t2
  | TSum (t1, t2) ->
      fprintf ppf "@[<h>(%a) + (%a)@]"
        print_local_type t1
        print_local_type t2

let print_choreo_ast ppf (Choreo.Prog prog) = print_stmt_block ppf prog; pp_print_newline ppf ()