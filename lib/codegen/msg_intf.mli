module type M = sig
  val emit_toplevel_init : string list -> Ppxlib.Parsetree.structure

  val emit_net_send
    :  src:string
    -> dst:string
    -> Parsetree.expression
    -> Parsetree.expression

  val emit_net_recv : src:string -> dst:string -> Parsetree.expression
end

module Msg_chan_intf : M
