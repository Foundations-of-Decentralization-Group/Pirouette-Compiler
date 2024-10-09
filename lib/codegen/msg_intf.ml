open Ppxlib

let spf = Printf.sprintf
let loc = { !Ast_helper.default_loc with loc_ghost = true }

module type M = sig
  val emit_toplevel_init : string list -> Parsetree.structure

  val emit_net_send
    :  src:string
    -> dst:string
    -> Parsetree.expression
    -> Parsetree.expression

  val emit_net_recv : src:string -> dst:string -> Parsetree.expression
end

module Msg_chan_intf : M = struct
  let emit_toplevel_init loc_ids =
    let loc_pairs =
      List.concat_map
        (fun a -> List.filter_map (fun b -> if a <> b then Some (a, b) else None) loc_ids)
        loc_ids
    in
    List.map
      (fun (a, b) ->
        [%stri
          let [%p Ast_builder.Default.pvar ~loc (spf "%s_%s_chan" a b)] =
            Domainslib.Chan.make_bounded 0
          ;;])
      loc_pairs
  ;;

  let emit_net_send ~src ~dst pexp =
    ignore dst;
    [%expr
      Domainslib.Chan.send
        [%e Ast_builder.Default.evar ~loc (spf "%s_%s_chan" src dst)]
        [%e pexp]]
  ;;

  let emit_net_recv ~src ~dst =
    [%expr
      Domainslib.Chan.recv [%e Ast_builder.Default.evar ~loc (spf "%s_%s_chan" src dst)]]
  ;;
end
