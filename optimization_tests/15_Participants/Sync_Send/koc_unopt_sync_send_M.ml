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
  Printf.printf "Starting process_%s...\n" "M";
  Send_receive.init_http_servers "M" ();
  (let process_M =
     let rec broadcast_unopt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"M")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "R" ->
           let rec x = 9 in
           let rec _unit_46 =
             let val_45 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_45 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           ()
       | "L" ->
           let rec x = 10 in
           let rec _unit_48 =
             let val_47 = x in
             match Lwt_main.run
                     (Send_receive.send_message ~location:"A"
                        ~data:(Marshal.to_string val_47 []))
             with
             | Ok () -> ()
             | Error msg -> failwith ("Send error: " ^ msg) in
           broadcast_unopt ()
       | _ -> failwith "Runtime Error: Unmatched label" in
     broadcast_unopt () in
   ignore process_M)