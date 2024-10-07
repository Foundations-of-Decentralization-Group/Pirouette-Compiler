module type M = sig
  val emit_net_send : string -> Ppxlib.expression -> Ppxlib.expression
  val emit_net_recv : string -> Ppxlib.expression
end
