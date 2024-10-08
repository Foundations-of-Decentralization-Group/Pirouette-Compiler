module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

module type Msg_intf = sig
  val emit_net_send : string -> Parsetree.expression -> Parsetree.expression
  val emit_net_recv : string -> Parsetree.expression
end

let loc = { !Ast_helper.default_loc with loc_ghost = true }

let rec emit_local_pexp (e : Local.expr) =
  match e with
  | Unit -> Ast_builder.Default.eunit ~loc
  | Val (Int i) -> Ast_builder.Default.eint ~loc i
  | Val (String s) -> Ast_builder.Default.estring ~loc s
  | Val (Bool b) -> Ast_builder.Default.ebool ~loc b
  | Var (VarId v) -> Ast_builder.Default.evar ~loc v
  | UnOp (Not, e) -> [%expr not [%e emit_local_pexp e]]
  | UnOp (Neg, e) -> [%expr -[%e emit_local_pexp e]]
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
      [ emit_local_pexp e1; emit_local_pexp e2 ]
  | Let (Local.VarId v, e1, e2) ->
    Ast_builder.Default.pexp_let
      ~loc
      Nonrecursive
      [ Ast_builder.Default.value_binding
          ~loc
          ~pat:(Ast_builder.Default.pvar ~loc v)
          ~expr:(emit_local_pexp e1)
      ]
      (emit_local_pexp e2)
  | Pair (e1, e2) -> [%expr [%e emit_local_pexp e1], [%e emit_local_pexp e2]]
  | Fst e -> [%expr fst [%e emit_local_pexp e]]
  | Snd e -> [%expr snd [%e emit_local_pexp e]]
  | Left e -> [%expr Either.Left [%e emit_local_pexp e]]
  | Right e -> [%expr Either.Right [%e emit_local_pexp e]]
  | Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(emit_local_ppat p)
            ~guard:None
            ~rhs:(emit_local_pexp e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (emit_local_pexp e) cases

and emit_local_ppat (p : Local.pattern) =
  match p with
  | Local.Default -> Ast_builder.Default.ppat_any ~loc
  | Local.Val (Int i) -> Ast_builder.Default.pint ~loc i
  | Local.Val (String s) -> Ast_builder.Default.pstring ~loc s
  | Local.Val (Bool b) -> Ast_builder.Default.pbool ~loc b
  | Local.Var (VarId v) -> Ast_builder.Default.pvar ~loc v
  | Local.Pair (p1, p2) -> [%pat? [%p emit_local_ppat p1], [%p emit_local_ppat p2]]
  | Local.Left p -> [%pat? Either.Left [%p emit_local_ppat p]]
  | Local.Right p -> [%pat? Either.Right [%p emit_local_ppat p]]
;;

let rec emit_fun_body
  (module Msg_intf : Msg_intf)
  (ps : Local.pattern list)
  (e : Net.expr)
  =
  match ps with
  | [] -> emit_net_pexp (module Msg_intf : Msg_intf) e
  | f :: ps ->
    Ast_builder.Default.pexp_fun
      ~loc
      Nolabel
      None
      (emit_local_ppat f)
      (emit_fun_body (module Msg_intf) ps e)

and emit_net_binding (module Msg_intf : Msg_intf) (s : Net.stmt) =
  match s with
  | Net.Assign (ps, e) ->
    (match ps with
     | [] -> failwith "Error: Empty pattern in assignment"
     | [ var ] ->
       Ast_builder.Default.value_binding
         ~loc
         ~pat:(emit_local_ppat var)
         ~expr:(emit_net_pexp (module Msg_intf) e)
     | f :: ps ->
       Ast_builder.Default.value_binding
         ~loc
         ~pat:(emit_local_ppat f)
         ~expr:(emit_fun_body (module Msg_intf) ps e))
  | _ ->
    Ast_builder.Default.value_binding
      ~loc
      ~pat:(Ast_builder.Default.punit ~loc)
      ~expr:(Ast_builder.Default.eunit ~loc)

and emit_net_pexp (module Msg_intf : Msg_intf) (e : Net.expr) =
  match e with
  | Net.Unit -> Ast_builder.Default.eunit ~loc
  | Net.Var (Local.VarId v) -> Ast_builder.Default.evar ~loc v
  | Net.Ret e -> emit_local_pexp e
  | Net.If (e1, e2, e3) ->
    Ast_builder.Default.pexp_ifthenelse
      ~loc
      (emit_net_pexp (module Msg_intf) e1)
      (emit_net_pexp (module Msg_intf) e2)
      (Some (emit_net_pexp (module Msg_intf) e3))
  | Net.Let (stmts, e) ->
    Ast_builder.Default.pexp_let
      ~loc
      Recursive
      (List.map (emit_net_binding (module Msg_intf)) stmts)
      (emit_net_pexp (module Msg_intf) e)
  | Net.FunDef (ps, e) -> emit_fun_body (module Msg_intf) ps e
  | Net.FunApp (e1, e2) ->
    [%expr
      [%e emit_net_pexp (module Msg_intf) e1] [%e emit_net_pexp (module Msg_intf) e2]]
  | Net.Pair (e1, e2) ->
    [%expr
      [%e emit_net_pexp (module Msg_intf) e1], [%e emit_net_pexp (module Msg_intf) e2]]
  | Net.Fst e -> [%expr Net.Fst [%e emit_net_pexp (module Msg_intf) e]]
  | Net.Snd e -> [%expr Net.Snd [%e emit_net_pexp (module Msg_intf) e]]
  | Net.Left e -> [%expr Either.Left [%e emit_net_pexp (module Msg_intf) e]]
  | Net.Right e -> [%expr Either.Right [%e emit_net_pexp (module Msg_intf) e]]
  | Net.Match (e, cases) ->
    let cases =
      List.map
        (fun (p, e) ->
          Ast_builder.Default.case
            ~lhs:(emit_local_ppat p)
            ~guard:None
            ~rhs:(emit_net_pexp (module Msg_intf) e))
        cases
    in
    Ast_builder.Default.pexp_match ~loc (emit_net_pexp (module Msg_intf) e) cases
  | Net.Send (e, LocId dst) ->
    Msg_intf.emit_net_send
      dst
      [%expr Marshal.to_string [%e emit_net_pexp (module Msg_intf) e] []]
  | Net.Recv (LocId src) -> [%expr Marshal.from_string [%e Msg_intf.emit_net_recv src] 0]
  | Net.ChooseFor (LabelId label, LocId loc_id, e) ->
    Ast_builder.Default.esequence
      ~loc
      [ Msg_intf.emit_net_send loc_id (Ast_builder.Default.estring ~loc label)
      ; emit_net_pexp (module Msg_intf) e
      ]
  | Net.AllowChoice (LocId loc_id, cases) ->
    let cases =
      List.map
        (fun (Local.LabelId label, e) ->
          Ast_builder.Default.case
            ~lhs:(Ast_builder.Default.pstring ~loc label)
            ~guard:None
            ~rhs:(emit_net_pexp (module Msg_intf) e))
        cases
    and default_case =
      Ast_builder.Default.case
        ~lhs:(Ast_builder.Default.ppat_any ~loc)
        ~guard:None
        ~rhs:[%expr failwith "Error: Unmatched label"]
    in
    Ast_builder.Default.pexp_match
      ~loc
      (Msg_intf.emit_net_recv loc_id)
      (cases @ [ default_case ])
;;
