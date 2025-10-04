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
  Printf.printf "Starting process_%s...\n" "C";
  Send_receive.init_http_servers "C" ();
  (let process_C =
     let rec gettimeofday arg = Unix.gettimeofday arg in
     let rec print_float arg = Stdlib.print_float arg in
     let rec sub_float arg = Stdlib.(-.) arg in
     let rec broadcast_unopt freq =
       match match Lwt_main.run (Send_receive.receive_message ~location:"C")
             with
             | Ok msg -> msg
             | Error msg -> failwith ("Receive error: " ^ msg)
       with
       | "L" -> let rec x = 10 in broadcast_unopt ()
       | "R" -> let rec x = 9 in ()
       | _ -> failwith "Runtime Error: Unmatched label" in
     let rec _unit_3 = broadcast_unopt () in () in
   ignore process_C)