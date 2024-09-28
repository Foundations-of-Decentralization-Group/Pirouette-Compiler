module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

let loc = { !Ast_helper.default_loc with loc_ghost = true }

let rec gen_local_expr (e : Local.expr) =
  match e with
  | Unit -> Ast_builder.Default.eunit ~loc
  | Val (Int i) -> Ast_builder.Default.eint ~loc i
  | Val (String s) -> Ast_builder.Default.estring ~loc s
  | Val (Bool b) -> Ast_builder.Default.ebool ~loc b
  | Var (Local.VarId v) -> Ast_builder.Default.evar ~loc v
  | UnOp (Local.Not, e) ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "not")
      [ gen_local_expr e ]
  | UnOp (Local.Neg, e) ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "~-")
      [ gen_local_expr e ]
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
      [ gen_local_expr e1; gen_local_expr e2 ]
  | Let (Local.VarId v, e1, e2) ->
    Ast_builder.Default.pexp_let
      ~loc
      Nonrecursive
      [ Ast_builder.Default.value_binding
          ~loc
          ~pat:(Ast_builder.Default.pvar ~loc v)
          ~expr:(gen_local_expr e1)
      ]
      (gen_local_expr e2)
  | Pair (e1, e2) ->
    Ast_builder.Default.pexp_tuple ~loc [ gen_local_expr e1; gen_local_expr e2 ]
  | Fst e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "fst")
      [ gen_local_expr e ]
  | Snd e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "snd")
      [ gen_local_expr e ]
  | Left e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (gen_local_expr e))
  | Right e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (gen_local_expr e))
  | Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(gen_local_pattern p)
            ~guard:None
            ~rhs:(gen_local_expr e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (gen_local_expr e) cases

and gen_local_pattern (p : Local.pattern) =
  match p with
  | Local.Default -> Ast_builder.Default.ppat_any ~loc
  | Local.Val (Int i) -> Ast_builder.Default.pint ~loc i
  | Local.Val (String s) -> Ast_builder.Default.pstring ~loc s
  | Local.Val (Bool b) -> Ast_builder.Default.pbool ~loc b
  | Local.Var (Local.VarId v) -> Ast_builder.Default.pvar ~loc v
  | Local.Pair (p1, p2) ->
    Ast_builder.Default.ppat_tuple ~loc [ gen_local_pattern p1; gen_local_pattern p2 ]
  | Local.Left p ->
    Ast_builder.Default.ppat_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (gen_local_pattern p))
  | Local.Right p ->
    Ast_builder.Default.ppat_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (gen_local_pattern p))
;;

let rec gen_net_expr (e : Net.expr) =
  let rec build_fun_body ps e =
    match ps with
    | [] -> gen_net_expr e
    | f :: ps ->
      Ast_builder.Default.pexp_fun
        ~loc
        Nolabel
        None
        (gen_local_pattern f)
        (build_fun_body ps e)
  in
  match e with
  | Net.Unit -> Ast_builder.Default.eunit ~loc
  | Net.Var (Local.VarId v) -> Ast_builder.Default.evar ~loc v
  | Net.Ret e -> gen_local_expr e
  | Net.If (e1, e2, e3) ->
    Ast_builder.Default.pexp_ifthenelse
      ~loc
      (gen_net_expr e1)
      (gen_net_expr e2)
      (Some (gen_net_expr e3))
  | Net.Let (stmts, e) ->
    Ast_builder.Default.pexp_let
      ~loc
      Nonrecursive
      (List.map
         (fun stmt ->
           match stmt with
           | Net.Assign (ps, e) ->
             (match ps with
              | [] -> failwith "Error: empty pattern in assignment"
              | [ var ] ->
                Ast_builder.Default.value_binding
                  ~loc
                  ~pat:(gen_local_pattern var)
                  ~expr:(gen_net_expr e)
              | f :: ps ->
                Ast_builder.Default.value_binding
                  ~loc
                  ~pat:(gen_local_pattern f)
                  ~expr:(build_fun_body ps e))
           | _ -> failwith "unimplemented")
         stmts)
      (gen_net_expr e)
  | Net.FunDef (ps, e) -> build_fun_body ps e
  | Net.FunApp (e1, e2) ->
    Ast_builder.Default.eapply ~loc (gen_net_expr e1) [ gen_net_expr e2 ]
  | Net.Pair (e1, e2) ->
    Ast_builder.Default.pexp_tuple ~loc [ gen_net_expr e1; gen_net_expr e2 ]
  | Net.Fst e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "fst")
      [ gen_net_expr e ]
  | Net.Snd e ->
    Ast_builder.Default.eapply
      ~loc
      (Ast_builder.Default.evar ~loc "snd")
      [ gen_net_expr e ]
  | Net.Left e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Left")))
      (Some (gen_net_expr e))
  | Net.Right e ->
    Ast_builder.Default.pexp_construct
      ~loc
      (Ast_builder.Default.Located.mk ~loc (Ldot (Lident "Either", "Right")))
      (Some (gen_net_expr e))
  | Net.Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(gen_local_pattern p)
            ~guard:None
            ~rhs:(gen_net_expr e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (gen_net_expr e) cases
  | _ -> failwith "unimplemented"
;;
