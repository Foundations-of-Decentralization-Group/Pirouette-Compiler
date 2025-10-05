open Http_pirc

[@@@ocaml.warning "-39"]
let () =
  let config_file_path : string = "koc_unopt_sync_send.yaml" in
  match Lwt_main.run (Config_parser.load_config config_file_path) with
  | Some cfg -> (Send_receive.config := (Some cfg); ())
  | None ->
      failwith
        (Printf.sprintf "Failed to load config from %s" config_file_path)
let () =
  Printf.printf "Starting process_%s...\n" "D";
  Send_receive.init_http_servers "D" ();
  (let process_D =
     let rec broadcast_unopt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"D")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "R" ->
           let rec x = 9 in
           let rec _unit_10 =
             let val_9 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_9 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           ()
       | "L" ->
           let rec x = 10 in
           let rec _unit_12 =
             let val_11 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_11 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           broadcast_unopt ()
       | _ -> failwith "Runtime Error: Unmatched label" in
     broadcast_unopt () in
   ignore process_D)