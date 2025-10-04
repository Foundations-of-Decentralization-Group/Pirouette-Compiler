open Http_pirc

[@@@ocaml.warning "-39"]
let () =
  let config_file_path : string = "koc_opt_sync_send.yaml" in
  match Lwt_main.run (Config_parser.load_config config_file_path) with
  | Some cfg -> (Send_receive.config := (Some cfg); ())
  | None ->
      failwith
        (Printf.sprintf "Failed to load config from %s" config_file_path)
let () =
  Printf.printf "Starting process_%s...\n" "I";
  Send_receive.init_http_servers "I" ();
  (let process_I =
     let rec gettimeofday arg = Unix.gettimeofday arg in
     let rec print_float arg = Stdlib.print_float arg in
     let rec sub_float arg = Stdlib.(-.) arg in
     let rec broadcast_opt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"I")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "L" ->
           let rec x = 10 in
           let rec _unit_38 =
             let val_37 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_37 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           broadcast_opt ()
       | "R" ->
           let rec x = 9 in
           let rec _unit_40 =
             let val_39 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_39 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           ()
       | _ -> failwith "Runtime Error: Unmatched label" in
     let rec _unit_41 = broadcast_opt () in () in
   ignore process_I)