module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

let loc = { !Ast_helper.default_loc with loc_ghost = true }

let rec emit_local_expr (e : Local.expr) =
  match e with
  | Unit -> Ast_builder.Default.eunit ~loc
  | Val (Int i) -> Ast_builder.Default.eint ~loc i
  | Val (String s) -> Ast_builder.Default.estring ~loc s
  | Val (Bool b) -> Ast_builder.Default.ebool ~loc b
  | Var (VarId v) -> Ast_builder.Default.evar ~loc v
  | UnOp (Not, e) ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "not")
      [ emit_local_expr e ]
  | UnOp (Neg, e) ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "~-")
      [ emit_local_expr e ]
  | BinOp (e1, op, e2) ->
    let op =
      match op with
      | Local.Plus -> "+"
      | Local.Minus -> "-"
      | Local.Times -> "*"
      | Local.Div -> "/"
      | Local.And -> "&&"
      | Local.Or -> "||"
      | Local.Eq -> "="
      | Local.Neq -> "<>"
      | Local.Lt -> "<"
      | Local.Leq -> "<="
      | Local.Gt -> ">"
      | Local.Geq -> ">="
    in
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc op)
      [ emit_local_expr e1; emit_local_expr e2 ]
  | Let (Local.VarId v, e1, e2) ->
    Ast_builder.Default.pexp_let
      ~loc
      Nonrecursive
      [ Ast_builder.Default.value_binding
          ~loc
          ~pat:(Ast_builder.Default.pvar ~loc v)
          ~expr:(emit_local_expr e1)
      ]
      (emit_local_expr e2)
  | Pair (e1, e2) ->
    Ast_builder.Default.pexp_tuple ~loc [ emit_local_expr e1; emit_local_expr e2 ]
  | Fst e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "fst")
      [ emit_local_expr e ]
  | Snd e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "snd")
      [ emit_local_expr e ]
  | Left e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (emit_local_expr e))
  | Right e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (emit_local_expr e))
  | Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(emit_local_pattern p)
            ~guard:None
            ~rhs:(emit_local_expr e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (emit_local_expr e) cases

and emit_local_pattern (p : Local.pattern) =
  match p with
  | Local.Default -> Ast_builder.Default.ppat_any ~loc
  | Local.Val (Int i) -> Ast_builder.Default.pint ~loc i
  | Local.Val (String s) -> Ast_builder.Default.pstring ~loc s
  | Local.Val (Bool b) -> Ast_builder.Default.pbool ~loc b
  | Local.Var (VarId v) -> Ast_builder.Default.pvar ~loc v
  | Local.Pair (p1, p2) ->
    Ast_builder.Default.ppat_tuple ~loc [ emit_local_pattern p1; emit_local_pattern p2 ]
  | Local.Left p ->
    Ast_builder.Default.ppat_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (emit_local_pattern p))
  | Local.Right p ->
    Ast_builder.Default.ppat_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (emit_local_pattern p))
;;

let rec build_fun_body
  (msg : (module Msg_intf.M))
  (ps : Local.pattern list)
  (e : Net.expr)
  =
  match ps with
  | [] -> emit_net_expr msg e
  | f :: ps ->
    Ast_builder.Default.pexp_fun
      ~loc
      Nolabel
      None
      (emit_local_pattern f)
      (build_fun_body msg ps e)

and emit_net_stmt (msg : (module Msg_intf.M)) (s : Net.stmt) =
  match s with
  | Net.Assign (ps, e) ->
    (match ps with
     | [] -> failwith "empty pattern in assignment"
     | [ var ] ->
       Ast_builder.Default.value_binding
         ~loc
         ~pat:(emit_local_pattern var)
         ~expr:(emit_net_expr msg e)
     | f :: ps ->
       Ast_builder.Default.value_binding
         ~loc
         ~pat:(emit_local_pattern f)
         ~expr:(build_fun_body msg ps e))
  | _ ->
    Ast_builder.Default.value_binding
      ~loc
      ~pat:(Ast_builder.Default.punit ~loc)
      ~expr:(Ast_builder.Default.eunit ~loc)

and emit_net_expr (msg : (module Msg_intf.M)) (e : Net.expr) =
  match e with
  | Net.Unit -> Ast_builder.Default.eunit ~loc
  | Net.Var (Local.VarId v) -> Ast_builder.Default.evar ~loc v
  | Net.Ret e -> emit_local_expr e
  | Net.If (e1, e2, e3) ->
    Ast_builder.Default.pexp_ifthenelse
      ~loc
      (emit_net_expr msg e1)
      (emit_net_expr msg e2)
      (Some (emit_net_expr msg e3))
  | Net.Let (stmts, e) ->
    Ast_builder.Default.pexp_let
      ~loc
      Nonrecursive
      (List.map (emit_net_stmt msg) stmts)
      (emit_net_expr msg e)
  | Net.FunDef (ps, e) -> build_fun_body msg ps e
  | Net.FunApp (e1, e2) ->
    Ast_builder.Default.eapply ~loc (emit_net_expr msg e1) [ emit_net_expr msg e2 ]
  | Net.Pair (e1, e2) ->
    Ast_builder.Default.pexp_tuple ~loc [ emit_net_expr msg e1; emit_net_expr msg e2 ]
  | Net.Fst e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "fst")
      [ emit_net_expr msg e ]
  | Net.Snd e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "snd")
      [ emit_net_expr msg e ]
  | Net.Left e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (emit_net_expr msg e))
  | Net.Right e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (emit_net_expr msg e))
  | Net.Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(emit_local_pattern p)
            ~guard:None
            ~rhs:(emit_net_expr msg e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (emit_net_expr msg e) cases
  | _ -> failwith "unimplemented"
;;
