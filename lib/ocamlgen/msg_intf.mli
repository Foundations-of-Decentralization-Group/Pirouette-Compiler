module type M = sig
  val emit_net_send : src:string -> dst:string -> ?switch_handle:Ppxlib.expression -> ?client:Ppxlib.expression -> pexp:Ppxlib.expression-> unit -> Ppxlib.expression
  val emit_net_recv : src:string -> dst:string -> Ppxlib.expression
end

