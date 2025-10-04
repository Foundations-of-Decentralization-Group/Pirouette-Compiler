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
  Printf.printf "Starting process_%s...\n" "E";
  Send_receive.init_http_servers "E" ();
  (let process_E =
     let rec gettimeofday arg = Unix.gettimeofday arg in
     let rec print_float arg = Stdlib.print_float arg in
     let rec sub_float arg = Stdlib.(-.) arg in
     let rec broadcast_opt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"E")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "L" ->
           ((match Lwt_main.run
                     (Send_receive.send_message ~location:"J" ~data:"L")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (match Lwt_main.run
                     (Send_receive.send_message ~location:"K" ~data:"L")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (let rec x = 10 in
             let rec _unit_18 =
               let val_17 = x in
               match Lwt_main.run
                       (Send_receive.send_message ~location:"A"
                          ~data:(Marshal.to_string val_17 []))
               with
               | Ok () -> ()
               | Error msg -> failwith ("Send error: " ^ msg) in
             broadcast_opt ()))
       | "R" ->
           ((match Lwt_main.run
                     (Send_receive.send_message ~location:"J" ~data:"R")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (match Lwt_main.run
                     (Send_receive.send_message ~location:"K" ~data:"R")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (let rec x = 9 in
             let rec _unit_20 =
               let val_19 = x in
               match Lwt_main.run
                       (Send_receive.send_message ~location:"A"
                          ~data:(Marshal.to_string val_19 []))
               with
               | Ok () -> ()
               | Error msg -> failwith ("Send error: " ^ msg) in
             ()))
       | _ -> failwith "Runtime Error: Unmatched label" in
     let rec _unit_21 = broadcast_opt () in () in
   ignore process_E)