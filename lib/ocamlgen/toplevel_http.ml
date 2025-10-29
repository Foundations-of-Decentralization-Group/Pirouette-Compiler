module Local = Ast_core.Local.M
module Net = Ast_core.Net.M
open Ppxlib

module Builder = Ast_builder.Make (struct
    let loc = { !Ast_helper.default_loc with loc_ghost = true }
  end)

let loc = Builder.loc
let spf = Printf.sprintf

module Msg_http_intf : Msg_intf.M = struct
  let emit_net_send ~src ~dst pexp =
    (* [%expr Domainslib.Chan.send [%e Builder.evar (spf "chan_%s_%s" src dst)] [%e pexp]]     *)
    [%expr
      let header_to_send =
        Send_receive.get_header [%e Ast_builder.Default.estring ~loc src]
      in
      let dst_ip =
        Send_receive.get_ip_address [%e Ast_builder.Default.estring ~loc dst]
      in
      let body_to_send = Send_receive.get_body [%e pexp] in
      let resp, body_resp =
        Cohttp_eio.Client.post
          ~sw
          ?body:body_to_send
          ?chunked:None
          ?headers:(Some header_to_send)
          client
          dst_ip
      in
      if Http.Status.compare resp.status `OK = 0
      then print_string @@ Eio.Buf_read.(parse_exn take_all) body_resp ~max_size:max_int
      else Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status]
  ;;

  (* let emit_net_recv ~src ~dst = *)
  (*   ignore src; *)
  (*   [%expr *)
  (*     match *)
  (*       Lwt_main.run *)
  (*         (Send_receive.receive_message *)
  (*            ~location:[%e Ast_builder.Default.estring ~loc dst]) *)
  (*     with *)
  (*     | Ok msg -> msg *)
  (*     | Error msg -> failwith ("Receive error: " ^ msg)] *)
  (* ;; *)

  let emit_net_recv ~src ~dst =
    ignore src;
    [%expr
      Send_receive.receive_message ~location:[%e Ast_builder.Default.estring ~loc dst]]
  ;;
end

let emit_toplevel_init _loc_ids config_file_path =
  [ [%stri
      print_endline "In here for testing";
      let config_file_path : string =
        [%e Ast_builder.Default.estring ~loc config_file_path]
      in
      match Lwt_main.run (Config_parser.load_config config_file_path) with
      | Some cfg ->
        Send_receive.config := Some cfg;
        (* Each process initializes its HTTP server in its own execution context *)
        ()
      | None -> failwith (Printf.sprintf "Failed to load config from %s" config_file_path)]
  ]
;;

let emit_toplevel_http
      out_chan
      (loc_ids : string list)
      (net_stmtblock_l : 'a Net.stmt_block list)
      (config_file_path : string)
  =
  let emit_domain_stri (loc_id : string) (net_stmts : 'a Net.stmt_block) =
    let main_expr = ref (Ast_builder.Default.eunit ~loc) in
    let rec emit_net_toplevel = function
      | [] -> !main_expr
      | stmt :: stmts ->
        (match Emit_core.emit_net_binding ~self_id:loc_id (module Msg_http_intf) stmt with
         | exception Emit_core.Main_expr e ->
           main_expr := e;
           emit_net_toplevel stmts
         | binding ->
           Ast_builder.Default.pexp_let
             ~loc
             Recursive
             [ binding ]
             (emit_net_toplevel stmts))
    in
    [%stri
      let () =
        Eio_main.run
        @@ fun env ->
        let dom_mgr = Eio.Stdenv.domain_mgr env in
        Eio.Fiber.all
          [ (fun () ->
              Eio.Domain_manager.run_raw
                dom_mgr
                (Send_receive.init_http_server
                   [%e Ast_builder.Default.estring ~loc loc_id]))
          ; (fun () ->
              let client = Cohttp_eio.Client.make ~https:None env#net in
              Eio.Switch.run ~name:"run_switch"
              @@ fun sw ->
              print_endline
                ("Starting process_%s...\n" ^ [%e Ast_builder.Default.estring ~loc loc_id]);
              (* Set the current location explicitly for this process *)
              let [%p Ast_builder.Default.pvar ~loc (spf "process_%s" loc_id)] =
                [%e emit_net_toplevel net_stmts]
              in
              ignore [%e Ast_builder.Default.evar ~loc (spf "process_%s" loc_id)])
          ]
      ;;]
  in
  let process_bindings = List.map2 emit_domain_stri loc_ids net_stmtblock_l in
  (* Add the warning suppression attribute *)
  let warning_attr =
    Ast_helper.Str.attribute
      ~loc
      { attr_name = { txt = "ocaml.warning"; loc }
      ; attr_payload = PStr [ [%stri "-39"] ]
      ; attr_loc = loc
      }
  in
  Pprintast.structure
    (Format.formatter_of_out_channel out_chan)
    ((warning_attr :: emit_toplevel_init loc_ids config_file_path) @ process_bindings)
;;

(* module Local = Ast_core.Local.M *)
(* module Net = Ast_core.Net.M *)
(* open Ppxlib *)

(* module Builder = Ast_builder.Make (struct *)
(*     let loc = { !Ast_helper.default_loc with loc_ghost = true } *)
(*   end) *)

(* let loc = Builder.loc *)
(* let spf = Printf.sprintf *)

(* module Msg_http_intf : Msg_intf.M = struct *)
(*   let emit_net_send ~src ~dst pexp = *)
(*     ignore src; *)
(*     [%expr *)
(*       match *)
(*         Lwt_main.run *)
(*           (Send_receive.send_message *)
(*              ~location:[%e Ast_builder.Default.estring ~loc dst] *)
(*              ~data:[%e pexp]) *)
(*       with *)
(*       | Ok () -> () *)
(*       | Error msg -> failwith ("Send error: " ^ msg)] *)
(*   ;; *)

(*   let emit_net_recv ~src ~dst = *)
(*     ignore src; *)
(*     [%expr *)
(*       match *)
(*         Lwt_main.run *)
(*           (Send_receive.receive_message *)
(*              ~location:[%e Ast_builder.Default.estring ~loc dst]) *)
(*       with *)
(*       | Ok msg -> msg *)
(*       | Error msg -> failwith ("Receive error: " ^ msg)] *)
(*   ;; *)
(* end *)

(* let emit_toplevel_init _loc_ids config_file_path = *)
(*   [ [%stri *)
(*     let () = *)
(*       print_endline "In here for testing"; *)
(*       let config_file_path : string = [%e Ast_builder.Default.estring ~loc config_file_path] in *)
(*       match Lwt_main.run (Config_parser.load_config config_file_path) with *)
(*       | Some cfg -> *)
(*         Send_receive.config := Some cfg; *)
(*         (\* Each process initializes its HTTP server in its own execution context *\) *)
(*         () *)
(*       | None -> *)
(*         failwith (Printf.sprintf "Failed to load config from %s" config_file_path) *)
(*     ;;] *)
(*   ] *)
(* ;; *)

(* let emit_toplevel_http *)
(*       out_chan *)
(*       (loc_ids : string list) *)
(*       (net_stmtblock_l : 'a Net.stmt_block list) *)
(*       (config_file_path : string) *)
(*   = *)
(*   let emit_domain_stri (loc_id : string) (net_stmts : 'a Net.stmt_block) = *)
(*     let main_expr = ref (Ast_builder.Default.eunit ~loc) in *)
(*     let rec emit_net_toplevel = function *)
(*       | [] -> !main_expr *)
(*       | stmt :: stmts -> *)
(*         (match Emit_core.emit_net_binding ~self_id:loc_id (module Msg_http_intf) stmt with *)
(*          | exception Emit_core.Main_expr e -> *)
(*            main_expr := e; *)
(*            emit_net_toplevel stmts *)
(*          | binding -> *)
(*            Ast_builder.Default.pexp_let *)
(*              ~loc *)
(*              Recursive *)
(*              [ binding ] *)
(*              (emit_net_toplevel stmts)) *)
(*     in *)
(*     [%stri *)
(*       let () = *)
(*         Printf.printf *)
(*           "Starting process_%s...\n" *)
(*           [%e Ast_builder.Default.estring ~loc loc_id]; *)
(*         (\* Set the current location explicitly for this process *\) *)
(*         Send_receive.init_http_servers [%e Ast_builder.Default.estring ~loc loc_id] *)
(*           (); *)
(*         let [%p Ast_builder.Default.pvar ~loc (spf "process_%s" loc_id)] = *)
(*           [%e emit_net_toplevel net_stmts] *)
(*         in *)
(*         ignore [%e Ast_builder.Default.evar ~loc (spf "process_%s" loc_id)] *)
(*       ;;] *)
(*   in *)
(*   let process_bindings = List.map2 emit_domain_stri loc_ids net_stmtblock_l in *)
(*   (\* Add the warning suppression attribute *\) *)
(*   let warning_attr = *)
(*     Ast_helper.Str.attribute *)
(*       ~loc *)
(*       { attr_name = { txt = "ocaml.warning"; loc } *)
(*       ; attr_payload = PStr [ [%stri "-39"] ] *)
(*       ; attr_loc = loc *)
(*       } *)
(*   in *)
(*   Pprintast.structure *)
(*     (Format.formatter_of_out_channel out_chan) *)
(*     ((warning_attr :: emit_toplevel_init loc_ids config_file_path) @ process_bindings) *)
(* ;; *)
