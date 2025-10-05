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
  Printf.printf "Starting process_%s...\n" "H";
  Send_receive.init_http_servers "H" ();
  (let process_H =
     let rec broadcast_unopt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"H")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "L" ->
           let rec x = 10 in
           let rec _unit_26 =
             let val_25 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_25 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           broadcast_unopt ()
       | "R" ->
           let rec x = 9 in
           let rec _unit_28 =
             let val_27 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_27 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           ()
       | _ -> failwith "Runtime Error: Unmatched label" in
     broadcast_unopt () in
   ignore process_H)