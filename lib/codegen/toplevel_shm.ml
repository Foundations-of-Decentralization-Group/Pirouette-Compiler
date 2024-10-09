module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

let spf = Printf.sprintf
let loc = { !Ast_helper.default_loc with loc_ghost = true }

let emit_domain_stri
  (module Msg : Msg_intf.M)
  (loc_id : string)
  (net_stmts : Net.stmt_block)
  =
  let rec emit_net_toplevel = function
    | [] -> Ast_builder.Default.eunit ~loc
    | stmt :: stmts ->
      Ast_builder.Default.pexp_let
        ~loc
        Recursive
        [ Emit_core.emit_net_binding ~self_id:loc_id (module Msg) stmt ]
        (emit_net_toplevel stmts)
  in
  [%stri
    let [%p Ast_builder.Default.pvar ~loc (spf "%s_domain" loc_id)] =
      Domain.spawn (fun _ -> [%e emit_net_toplevel net_stmts])
    ;;]
;;

let emit_toplevel_shm
  ppf
  (module Msg : Msg_intf.M)
  (loc_ids : string list)
  (net_stmtblock_l : Net.stmt_block list)
  =
  let domain_defs =
    List.map2 (emit_domain_stri (module Msg)) loc_ids net_stmtblock_l
  in
  Pprintast.structure ppf (Msg.emit_toplevel_init loc_ids @ domain_defs)
;;
