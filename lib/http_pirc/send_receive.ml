open Cohttp_eio

(* Global config reference *)
let config = ref None

(* Message queues for each location *)
let loc_to_address = Hashtbl.create 10
let message_queues : (string, string Eio.Stream.t) Hashtbl.t = Hashtbl.create 10

(* Helper to get location config *)
let get_location_config location =
  match !config with
  | None -> Error "Config not initialized. Call init() first"
  | Some cfg ->
    (match
       List.find_opt
         (fun loc -> loc.Config_parser.location = location)
         cfg.Config_parser.locations
     with
     | Some loc_config -> Ok loc_config
     | None -> Error ("Unknown location: " ^ location))
;;

let get_body string_to_send =
  let body_to_send = Some (Body.of_string string_to_send) in
  body_to_send
;;

let get_ip_address ~location =
  match get_location_config location with
  | Ok loc_config -> Uri.of_string loc_config.http_address
  | _ -> Uri.empty
;;

let get_header ~location_name =
  let new_header = Http.Header.init () in
  let header_to_send = Http.Header.add new_header "Location" location_name in
  header_to_send
;;

(* Function to marshal data *)
let marshal_data data =
  try
    let result = Marshal.to_string data [] in
    result
  with
  | e -> raise e
;;

(* Function to unmarshal data *)
let unmarshal_data data_str =
  try
    if String.length data_str = 0
    then Error "Empty data string"
    else (
      let result = Marshal.from_string data_str 0 in
      Ok result)
  with
  | e -> Error ("Unmarshal error: " ^ Printexc.to_string e)
;;

(* This is the handler for incoming http requests *)
let handler _socket request body =
  let x : Cohttp_eio.Body.t = body in
  let sender_body = Eio.Buf_read.(parse_exn take_all) ~max_size:max_int x in
  let recv_headers = Http.Request.headers request in
  let sender_location = Http.Header.get recv_headers "Location" in
  match sender_location with
  | None ->
    Cohttp_eio.Server.respond_string
      ~status:`Precondition_failed
      ~body:"Error message - Sender location not found"
      ()
  | Some unwrapped_sender_location ->
    let indexed_queue = Hashtbl.find_opt message_queues unwrapped_sender_location in
    print_endline "The key already exists";
    (match indexed_queue with
     | Some result_queue ->
       let get_sender_body input_string =
         match sender_body with
         | "L" ->
           print_endline "The L branch was taken";
           let string_to_print = input_string in
           string_to_print
         | "R" ->
           print_endline "The R branch was taken";
           let string_to_print = input_string in
           string_to_print
         | _ ->
           print_endline "Looks like we have something that had been marshalled";
           let string_to_print = Marshal.from_string input_string 0 in
           string_of_int string_to_print
       in
       let string_to_print = get_sender_body sender_body in
       Eio.traceln "%s" string_to_print;
       Eio.traceln "%s" unwrapped_sender_location;
       Eio.Stream.add result_queue sender_body;
       Cohttp_eio.Server.respond_string
         ~status:`OK
         ~body:"Added to Htbl ; existing key"
         ()
     | None ->
       Hashtbl.add message_queues unwrapped_sender_location (Eio.Stream.create 10);
       let indexed_queue = Hashtbl.find message_queues unwrapped_sender_location in
       (* print_endline *)
       (*   ("This value was put inside the queue : " *)
       (*    ^ sender_body *)
       (*    ^ " " *)
       (*    ^ unwrapped_sender_location); *)
       print_endline "The key has to be made";
       let get_sender_body input_string =
         match sender_body with
         | "L" ->
           print_endline "The L branch was taken";
           let string_to_print = input_string in
           string_to_print
         | "R" ->
           print_endline "The R branch was taken";
           let string_to_print = input_string in
           string_to_print
         | _ ->
           print_endline "Looks like we have something that had been marshalled";
           let string_to_print = Marshal.from_string input_string 0 in
           string_of_int string_to_print
       in
       let string_to_print = get_sender_body sender_body in
       Eio.traceln "%s" string_to_print;
       Eio.traceln "%s" unwrapped_sender_location;
       Eio.Stream.add indexed_queue sender_body;
       Cohttp_eio.Server.respond_string
         ~status:`OK
         ~body:("Added to Htbl ; new key - sender body " ^ sender_body)
         ())
;;

(* (match Http.Request.meth request with *)
(*  | `GET -> print_endline "The get method was called" *)
(*  | `POST -> *)
(*    let cond = Http.Request.has_body request in *)
(*    (match cond with *)
(*     | `No -> print_endline "There is no body" *)
(*     | `Unknown -> print_endline "Unknown" *)
(*     | `Yes -> print_endline "Yes ; there is a body good news") *)
(*  | _ -> print_endline "Something else was called"); *)
(* (match Http.Request.resource request with *)
(*  | "/" -> Cohttp_eio.Server.respond_string ~status:`OK ~body:"" () *)
(*  | "/html" -> *)
(*    (\* Use a plain flow to test chunked encoding *\) *)
(*    let body = Eio.Flow.string_source "" in *)
(*    Cohttp_eio.Server.respond *)
(*      () *)
(*      ~status:`OK *)
(*      ~headers:(Http.Header.of_list [ "content-type", "text/html" ]) *)
(*      ~body *)
(*  | "/post" -> *)
(*    print_endline "This is the post request from the client"; *)
(*    Cohttp_eio.Server.respond_string *)
(*      ~status:`OK *)
(*      ~body:"This is the post method being invoked" *)
(*      () *)
(*  | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"" ()) *)

(* This is to setup config ; ie update the reference and pull the values from that into a hashtable *)
let setup_config_file () =
  (* First collect all original addresses *)
  match !config with
  | None -> ()
  | Some cfg ->
    List.iter
      (fun loc_cfg ->
         Hashtbl.add
           loc_to_address
           loc_cfg.Config_parser.location
           loc_cfg.Config_parser.http_address)
      cfg.Config_parser.locations;
    (* Now create a new config with the updated addresses *)
    let new_locations =
      List.map
        (fun loc_cfg ->
           { loc_cfg with
             Config_parser.http_address =
               Hashtbl.find loc_to_address loc_cfg.Config_parser.location
           })
        cfg.Config_parser.locations
    in
    (* Update the config reference *)
    config := Some { Config_parser.locations = new_locations }
;;

(* Initialize HTTP server for this location *)
let init_http_server current_location () =
  print_endline "Right before setup_config_file";
  let () = setup_config_file () in
  print_endline "After setup_config_file";
  match get_location_config current_location with
  | Error msg ->
    failwith
      ("location config" ^ current_location ^ "not found inside init_http_location" ^ msg)
  | Ok loc_config ->
    print_endline "We got a config debug statement";
    let uri = Uri.of_string loc_config.Config_parser.http_address in
    (* let _path = Uri.path uri in *)
    (* (\* Extract port from URI or use a default *\) *)
    print_endline "Got the URI";
    let port_to_use : int =
      match Uri.port uri with
      | Some p -> p (* Use original port *)
      | None -> 8080
      (* Default port *)
    in
    (* let host = Uri.host uri |> Option.value ~default:"localhost" in *)
    (* let new_address = Printf.sprintf "http://%s:%d%s" host port_to_use path in *)
    (* Update our mapping with the new address *)
    (* Hashtbl.replace loc_to_address loc_config.Config_parser.location new_address; *)
    print_endline ("Starting an HTTP server for this specific node" ^ current_location);
    (* The following statement sets up logs for debugging *)
    let () = Logs.set_reporter (Logs_fmt.reporter ())
    and () = Logs.Src.set_level Cohttp_eio.src (Some Debug) in
    let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex) in
    (* This runs the Eio event loop for the server *)
    let () =
      print_endline "Just before the EIO main loop";
      let port = ref port_to_use in
      print_endline ("This is the port that is being used " ^ string_of_int port_to_use);
      Arg.parse
        [ "-p", Arg.Set_int port, " Listening port number(8080 by default)" ]
        ignore
        "An HTTP/1.1 server";
      Eio_main.run
      @@ fun env ->
      print_endline "Inside the EIO main loop";
      Eio.Switch.run
      @@ fun sw ->
      print_endline "Inside the EIO switch run";
      let socket =
        Eio.Net.listen
          env#net
          ~sw
          ~backlog:128
          ~reuse_addr:true
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
      and server = Cohttp_eio.Server.make ~callback:handler () in
      print_endline "After the and server part";
      Cohttp_eio.Server.run socket server ~on_error:log_warning
    in
    print_endline "Finished";
    ()
;;

let rec receive_message ~location =
  let key_for_table = location in
  let stream_handle_option = Hashtbl.find_opt message_queues key_for_table in
  (* let received_message = Eio.Stream.take stream_for_message in received_message *)
  match stream_handle_option with
  | Some stream_associated_key -> Eio.Stream.take stream_associated_key
  | None -> receive_message ~location
;;
