val emit_local_pexp : Ast.Local.expr -> Parsetree.expression
val emit_local_ppat : Ast.Local.pattern -> Parsetree.pattern

val emit_net_fun_body
  :  self_id:string
  -> (module Msg_intf.M)
  -> Ast.Local.pattern list
  -> Ast.Net.expr
  -> Parsetree.expression

val emit_net_binding
  :  self_id:string
  -> (module Msg_intf.M)
  -> Ast.Net.stmt
  -> Parsetree.value_binding

val emit_net_pexp
  :  self_id:string
  -> (module Msg_intf.M)
  -> Ast.Net.expr
  -> Parsetree.expression