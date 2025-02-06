module type M = sig
  val emit_toplevel_defs : string list -> Ppxlib.value_binding list
  val emit_net_send : src:string -> dst:string -> Ppxlib.expression -> Ppxlib.expression
  val emit_net_recv : src:string -> dst:string -> Ppxlib.expression
end

module Msg_chan_intf : M
module Msg_http_intf : M
