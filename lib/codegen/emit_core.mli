exception Main_expr of Parsetree.expression

val emit_local_pexp : 'a Ast_core.Local.M.expr -> Parsetree.expression
val emit_local_ppat : 'a Ast_core.Local.M.pattern -> Parsetree.pattern

val emit_net_fun_body
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Local.M.pattern list
  -> 'a Ast_core.Net.M.expr
  -> Parsetree.expression

val emit_net_binding
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Net.M.stmt
  -> Parsetree.value_binding

val emit_net_pexp
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Net.M.expr
  -> Parsetree.expression
