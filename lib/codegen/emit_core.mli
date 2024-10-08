module type Msg_intf = sig
  val emit_net_send : string -> Parsetree.expression -> Parsetree.expression
  val emit_net_recv : string -> Parsetree.expression
end

val loc : Warnings.loc
val emit_local_pexp : Ast.Local.expr -> Parsetree.expression
val emit_local_ppat : Ast.Local.pattern -> Parsetree.pattern

val emit_fun_body
  :  (module Msg_intf)
  -> Ast.Local.pattern list
  -> Ast.Net.expr
  -> Parsetree.expression

val emit_net_binding : (module Msg_intf) -> Ast.Net.stmt -> Parsetree.value_binding
val emit_net_pexp : (module Msg_intf) -> Ast.Net.expr -> Parsetree.expression
