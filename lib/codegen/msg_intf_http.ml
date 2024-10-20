open Ppxlib
open Lwt.Infix
open Send_receive

let spf = Printf.sprintf
let loc = { !Ast_helper.default_loc with loc_ghost = true }

module type M = sig
  val emit_toplevel_init : string list -> Parsetree.structure

  val emit_net_send
    :  src:string
    -> dst:string
    -> Parsetree.expression
    -> Parsetree.expression Lwt.t

  val emit_net_recv : src:string -> dst:string -> Parsetree.expression Lwt.t
end

module Msg_http_intf : M = struct
  let emit_toplevel_init _loc_ids = []

  let emit_net_send ~src ~dst pexp =
    let url = spf "http://%s/send" dst in
    let data = Marshal.to_string pexp [] in
    send_message ~url ~data
    >>= function
    | Ok () -> Lwt.return pexp
    | Error msg -> failwith ("Send error: " ^ msg)
  ;;

  let emit_net_recv ~src ~dst =
    let url = spf "http://%s/receive" src in
    receive_message ~url
    >>= function
    | Ok data -> Lwt.return data
    | Error msg -> failwith ("Receive error: " ^ msg)
  ;;
end