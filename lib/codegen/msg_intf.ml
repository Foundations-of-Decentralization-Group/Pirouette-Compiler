module type M = sig
  val emit_net_send : string -> Parsetree.expression -> Parsetree.expression
  val emit_net_recv : string -> Parsetree.expression
end
