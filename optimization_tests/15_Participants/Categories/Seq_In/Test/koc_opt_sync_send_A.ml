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
  Printf.printf "Starting process_%s...\n" "A";
  Send_receive.init_http_servers "A" ();
  (let process_A =
     let rec gettimeofday arg = Unix.gettimeofday arg in
     let rec print_float arg = Stdlib.print_float arg in
     let rec sub_float arg = Stdlib.(-.) arg in
     let rec broadcast_opt freq =
       if freq > 0
       then
         ((match Lwt_main.run
                   (Send_receive.send_message ~location:"B" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"C" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (let rec result_B =
             Marshal.from_string
               (match Lwt_main.run
                        (Send_receive.receive_message ~location:"A")
                with
                | Ok msg -> msg
                | Error msg -> failwith ("Receive error: " ^ msg)) 0 in
           let rec result_C =
             Marshal.from_string
               (match Lwt_main.run
                        (Send_receive.receive_message ~location:"A")
                with
                | Ok msg -> msg
                | Error msg -> failwith ("Receive error: " ^ msg)) 0 in
           broadcast_opt (freq - 1)))
       else
         ((match Lwt_main.run
                   (Send_receive.send_message ~location:"B" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"C" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (let rec result_B =
             Marshal.from_string
               (match Lwt_main.run
                        (Send_receive.receive_message ~location:"A")
                with
                | Ok msg -> msg
                | Error msg -> failwith ("Receive error: " ^ msg)) 0 in
           let rec result_C =
             Marshal.from_string
               (match Lwt_main.run
                        (Send_receive.receive_message ~location:"A")
                with
                | Ok msg -> msg
                | Error msg -> failwith ("Receive error: " ^ msg)) 0 in
           ())) in
     let rec start_time = gettimeofday () in
     let rec _unit_1 = broadcast_opt 10 in
     let rec end_time = gettimeofday () in
     let rec time_diff = (sub_float end_time) start_time in
     print_float time_diff in
   ignore process_A)