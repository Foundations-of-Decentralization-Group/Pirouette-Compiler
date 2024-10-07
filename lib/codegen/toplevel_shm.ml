module Local = Ast.Local
module Net = Ast.Net
open Ppxlib

let loc = Emit_ocaml.loc

let emit_domain_stri (loc_id : string) (net_stmts : Net.stmt_block) =
  let module Msg : Msg_intf.M = struct
    let emit_net_send dst pexp =
      [%expr
        Domainslib.Chan.send
          [%e Ast_builder.Default.evar ~loc (loc_id ^ dst ^ "_chan")]
          [%e pexp]]
    ;;

    let emit_net_recv src =
      [%expr
        Domainslib.Chan.recv [%e Ast_builder.Default.evar ~loc (src ^ loc_id ^ "_chan")]]
    ;;
  end
  in
  let rec emit_net_stmtblock = function
    | [] -> Ast_builder.Default.eunit ~loc
    | stmt :: stmts ->
      let binding =
        match stmt with
        | Net.Assign (ps, e) ->
          (match ps with
           | [] -> failwith "empty pattern in assignment"
           | [ var ] ->
             Ast_builder.Default.value_binding
               ~loc
               ~pat:(Emit_ocaml.emit_local_pattern var)
               ~expr:(Emit_ocaml.emit_net_expr (module Msg) e)
           | f :: ps ->
             Ast_builder.Default.value_binding
               ~loc
               ~pat:(Emit_ocaml.emit_local_pattern f)
               ~expr:(Emit_ocaml.build_fun_body (module Msg) ps e))
        | _ ->
          Ast_builder.Default.value_binding
            ~loc
            ~pat:(Ast_builder.Default.punit ~loc)
            ~expr:(Ast_builder.Default.eunit ~loc)
      in
      Ast_builder.Default.pexp_let
        ~loc
        Nonrecursive
        [ binding ]
        (emit_net_stmtblock stmts)
  in
  [%stri
    let [%p Ast_builder.Default.pvar ~loc (loc_id ^ "_domain")] =
      Domain.spawn (fun _ -> [%e emit_net_stmtblock net_stmts])
    ;;]
;;
