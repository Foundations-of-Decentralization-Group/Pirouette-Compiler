module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

let spf = Printf.sprintf
let loc = Emit_core.loc

let emit_domain_stri (loc_id : string) (net_stmts : Net.stmt_block) =
  let module Msg_chan : Emit_core.Msg_intf = struct
    let emit_net_send dst pexp =
      [%expr
        Domainslib.Chan.send
          [%e Ast_builder.Default.evar ~loc (spf "%s_%s_chan" dst loc_id)]
          [%e pexp]]
    ;;

    let emit_net_recv src =
      [%expr
        Domainslib.Chan.recv
          [%e Ast_builder.Default.evar ~loc (spf "%s_%s_chan" loc_id src)]]
    ;;
  end
  in
  let rec emit_net_toplevel = function
    | [] -> Ast_builder.Default.eunit ~loc
    | stmt :: stmts ->
      Ast_builder.Default.pexp_let
        ~loc
        Recursive
        [ Emit_core.emit_net_binding (module Msg_chan) stmt ]
        (emit_net_toplevel stmts)
  in
  [%stri
    let [%p Ast_builder.Default.pvar ~loc (spf "%s_domain" loc_id)] =
      Domain.spawn (fun _ -> [%e emit_net_toplevel net_stmts])
    ;;]
;;

let emit_ocaml_shm ppf (loc_ids : string list) (net_stmtblock_l : Net.stmt_block list) =
  let loc_pairs =
    List.concat_map
      (fun a -> List.filter_map (fun b -> if a <> b then Some (a, b) else None) loc_ids)
      loc_ids
  in
  let chan_defs =
    List.map
      (fun (a, b) ->
        [%stri
          let [%p Ast_builder.Default.pvar ~loc (spf "%s_%s_chan" a b)] =
            Domainslib.Chan.make_bounded 0
          ;;])
      loc_pairs
  and domain_defs = List.map2 emit_domain_stri loc_ids net_stmtblock_l in
  Pprintast.structure ppf (chan_defs @ domain_defs)
;;
