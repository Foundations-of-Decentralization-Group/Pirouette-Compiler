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
  Printf.printf "Starting process_%s...\n" "D";
  Send_receive.init_http_servers "D" ();
  (let process_D =
     let rec gettimeofday arg = Unix.gettimeofday arg in
     let rec print_float arg = Stdlib.print_float arg in
     let rec sub_float arg = Stdlib.(-.) arg in
     let rec broadcast_opt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"D")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "R" ->
           ((match Lwt_main.run
                     (Send_receive.send_message ~location:"H" ~data:"R")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (match Lwt_main.run
                     (Send_receive.send_message ~location:"I" ~data:"R")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (let rec x = 9 in
             let rec _unit_13 =
               let val_12 = x in
               match Lwt_main.run
                       (Send_receive.send_message ~location:"A"
                          ~data:(Marshal.to_string val_12 []))
               with
               | Ok () -> ()
               | Error msg -> failwith ("Send error: " ^ msg) in
             ()))
       | "L" ->
           ((match Lwt_main.run
                     (Send_receive.send_message ~location:"H" ~data:"L")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (match Lwt_main.run
                     (Send_receive.send_message ~location:"I" ~data:"L")
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg));
            (let rec x = 10 in
             let rec _unit_15 =
               let val_14 = x in
               match Lwt_main.run
                       (Send_receive.send_message ~location:"A"
                          ~data:(Marshal.to_string val_14 []))
               with
               | Ok () -> ()
               | Error msg -> failwith ("Send error: " ^ msg) in
             broadcast_opt ()))
       | _ -> failwith "Runtime Error: Unmatched label" in
     let rec _unit_16 = broadcast_opt () in () in
   ignore process_D)