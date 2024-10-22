module type M = sig
  val emit_toplevel_init : string list -> Ppxlib.structure

  val emit_net_send
    :  src:string
    -> dst:string
    -> Ppxlib.expression
    -> Ppxlib.expression

  val emit_net_recv : src:string -> dst:string -> Ppxlib.expression
end

module Msg_chan_intf : M
