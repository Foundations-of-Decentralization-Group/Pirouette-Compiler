open Http_pirc

[@@@ocaml.warning "-39"]
let () =
  let config_file_path : string = "koc_unopt_ss.yaml" in
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
     let rec broadcast_unopt freq =
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
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"D" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"E" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"F" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"G" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"H" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"I" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"J" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"K" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"L" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"M" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"N" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"O" ~data:"L")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          broadcast_unopt (freq - 1))
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
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"D" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"E" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"F" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"G" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"H" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"I" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"J" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"K" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"L" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"M" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"N" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (match Lwt_main.run
                   (Send_receive.send_message ~location:"O" ~data:"R")
           with
           | Ok () -> ()
           | Error msg -> failwith ("Send error: " ^ msg));
          (let rec result_O =
             Marshal.from_string
               (match Lwt_main.run
                        (Send_receive.receive_message ~location:"A")
                with
                | Ok msg -> msg
                | Error msg -> failwith ("Receive error: " ^ msg)) 0 in
           ())) in
     let rec start_time = gettimeofday () in
     let rec _unit_1 = broadcast_unopt 10000 in
     let rec end_time = gettimeofday () in
     let rec time_diff = (sub_float end_time) start_time in
     print_float time_diff in
   ignore process_A)