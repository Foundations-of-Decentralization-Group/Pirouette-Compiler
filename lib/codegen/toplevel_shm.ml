module Local = Ast_core.Local.M
module Net = Ast_core.Net.M
open Ppxlib

let spf = Printf.sprintf
let loc = { !Ast_helper.default_loc with loc_ghost = true }

let emit_toplevel_shm
  out_chan
  (module Msg : Msg_intf.M)
  (loc_ids : string list)
  (net_stmtblock_l : 'a Net.stmt_block list)
  =
  let emit_domain_bindings
    (loc_ids : string list)
    (net_stmtblock_l : 'a Net.stmt_block list)
    =
    let main_expr = ref (Ast_builder.Default.eunit ~loc) in
    let rec emit_net_toplevel loc_id stmts =
      match stmts with
      | [] -> !main_expr
      | stmt :: stmts ->
        (match Emit_core.emit_net_binding ~self_id:loc_id (module Msg) stmt with
         | exception Emit_core.Main_expr e ->
           main_expr := e;
           emit_net_toplevel loc_id stmts
         | binding ->
           Ast_builder.Default.pexp_let
             ~loc
             Recursive
             [ binding ]
             (emit_net_toplevel loc_id stmts))
    in
    List.map2
      (fun loc_id net_stmts ->
        Ast_builder.Default.value_binding
          ~loc
          ~pat:(Ast_builder.Default.pvar ~loc (spf "domain_%s" loc_id))
          ~expr:[%expr Domain.spawn (fun _ -> [%e emit_net_toplevel loc_id net_stmts])])
      loc_ids
      net_stmtblock_l
  in
  let rec emit_domain_join_seq = function
    | [] -> assert false
    | [ loc_id ] ->
      [%expr Domain.join [%e Ast_builder.Default.evar ~loc (spf "domain_%s" loc_id)]]
    | loc_id :: loc_ids ->
      Ast_builder.Default.pexp_sequence
        ~loc
        [%expr Domain.join [%e Ast_builder.Default.evar ~loc (spf "domain_%s" loc_id)]]
        (emit_domain_join_seq loc_ids)
  in
  let ppf = Format.formatter_of_out_channel chan in
  Pprintast.structure
    ppf
    [ Ast_builder.Default.pstr_eval
        ~loc
        (Ast_builder.Default.pexp_let
           ~loc
           Nonrecursive
           (Msg.emit_toplevel_init loc_ids)
           (Ast_builder.Default.pexp_let
              ~loc
              Nonrecursive
              (emit_domain_bindings loc_ids net_stmtblock_l)
              (emit_domain_join_seq loc_ids)))
        []
    ];
  Format.pp_print_newline ppf ()
;;
