(** HTTP-based message passing for Pirouette choreographies.
    
    This module provides the runtime support for sending and receiving messages
    between distributed endpoints using HTTP as the communication protocol.
    
    The message passing functions {!send_message} and {!receive_message} return 
    Lwt promises (asynchronous operations). *)

open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Lwt.Syntax (* For let+ and let* syntax *)

(** Global config reference, initialized by the compiler generated code. *)
let config = ref None

(** message_queues is storing received messages for each location, keyed by location name.
    
    The hash table is initialized with capacity for 10 locations and will grow 
    automatically as needed. *)
let message_queues = Hashtbl.create 10
(** message_condition is the condition variable for signaling message arrival to waiting receivers. *)
let message_condition = Lwt_condition.create ()

(** Helper to get location config; [get_location_config location] is [Ok config] containing the HTTP address
    configuration for the [location] if [location] exists in the configuration,
    and [Error msg] if the configuration is not initialized or [location] is unknown.
    
    Requires: Configuration must be initialized before calling this function.  *)
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
(* ASK ANDREW if he wants a quick explination of marshaling because i didnt know what 
  it was meant marshalling data is taking ocaml values to/from binary representation *)
(** [marshal_data data] is the marshaled string representation of [data].
    
    Requires: [data] must be a valid OCaml value that can be marshaled.
    Raises: Exception if marshaling fails. *)
let marshal_data data =
  try
    let result = Marshal.to_string data [] in
    result
  with
  | e -> raise e
;;

(** [unmarshal_data data_str] is [Ok value] containing the unmarshaled OCaml value
    if [data_str] is a valid marshaled string, and [Error msg] if [data_str] is
    empty or unmarshaling fails.
    
    Requires: [data_str] must be a marshaled OCaml value. *)
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

(** Internal Mapping Function;[map_send_location location] is the actual location name to use for sending.
    Currently returns [location] unchanged. *)
let map_send_location location = location
(** Internal Mapping Function;[map_receive_location location] is the actual location name to use for receiving.
    Currently returns [location] unchanged,*)
let map_receive_location location = location

(** Function to send a message;  [send_message ~location ~data] is [Ok ()] if the message [data] is successfully
    sent to [location], and [Error msg] if sending fails due to configuration errors,
    network issues, or HTTP errors.
    
    The function marshals [data], sends it via HTTP POST to the configured address
    for [location], and waits for acknowledgment.
    
    Requires: 
    - Configuration must be initialized
    - [location] must exist in the configuration
    - [data] must be marshalable *) 
let send_message ~location ~data =
  let actual_location = map_send_location location in
  match get_location_config actual_location with
  | Error msg -> Lwt.return_error msg
  | Ok loc_config ->
    (try
       let marshaled_data = marshal_data data in
       (* Print the data being sent *)
       Printf.printf
         "DATA BEING SENT: %s\n"
         (match data with
          | _ when Obj.is_int (Obj.repr data) -> "INT: " ^ string_of_int (Obj.magic data)
          | _ when Obj.tag (Obj.repr data) = Obj.string_tag -> "STRING: " ^ Obj.magic data
          | _ ->
            (try
               if Obj.is_block (Obj.repr data) && Obj.tag (Obj.repr data) = 0
               then (
                 match Obj.magic data with
                 | Ok v ->
                   if Obj.is_int (Obj.repr v)
                   then "Ok(" ^ string_of_int (Obj.magic v) ^ ")"
                   else if Obj.tag (Obj.repr v) = Obj.string_tag
                   then "Ok(\"" ^ Obj.magic v ^ "\")"
                   else "Ok(<value>)"
                 | Error msg -> "Error(\"" ^ msg ^ "\")")
               else "<complex data>"
             with
             | _ -> "<unprintable data>"));
       flush stdout;
       let headers = Header.init_with "Content-Type" "application/octet-stream" in
       let body = Cohttp_lwt.Body.of_string marshaled_data in
       (* No need to create URI variable since it's not used *)
       Client.post ~headers ~body (Uri.of_string loc_config.http_address)
       >>= fun (resp, body) ->
       let status = resp |> Response.status |> Code.code_of_status in
       Cohttp_lwt.Body.drain_body body
       >>= fun () ->
       if status = 200
       then Lwt.return_ok ()
       else
         Lwt.return_error ("Failed to send message, status code: " ^ string_of_int status)
     with
     | e -> Lwt.return_error ("Send error: " ^ Printexc.to_string e))
;;

(** Initialize HTTP servers for all locations; [init_http_servers current_location ()] initializes HTTP servers for [current_location].
    
    Starts an HTTP server that listens for incoming messages at the configured address
    for [current_location]. Received messages are stored in the message queue and waiting
    receivers are notified.
    
    Requires:
    - Configuration must be initialized
    - [current_location] must exist in the configuration
    - The configured port must be available *)
let init_http_servers current_location () =
  match !config with
  | None -> ()
  | Some cfg ->
    (* Create a mapping of location to http_address for all locations *)
    let loc_to_address = Hashtbl.create 10 in
    (* First collect all original addresses *)
    List.iter
      (fun loc_cfg ->
         Hashtbl.add
           loc_to_address
           loc_cfg.Config_parser.location
           loc_cfg.Config_parser.http_address)
      cfg.Config_parser.locations;
    (* Filter locations to only include the current location if specified *)
    let locations_to_serve =
      List.filter
        (fun loc_cfg -> loc_cfg.Config_parser.location = current_location)
        cfg.Config_parser.locations
    in
    Printf.printf
      "Starting servers for locations: %s\n"
      (String.concat
         ", "
         (List.map (fun l -> l.Config_parser.location) locations_to_serve));
    let server_promises =
      List.map
        (fun loc_cfg ->
           let uri = Uri.of_string loc_cfg.Config_parser.http_address in
           let path = Uri.path uri in
           (* Extract port from URI or use a default *)
           let port =
             match Uri.port uri with
             | Some p -> p (* Use original port *)
             | None -> 8080 (* Default port *)
           in
           (* Create new address with offset port for client connections *)
           let host = Uri.host uri |> Option.value ~default:"localhost" in
           let new_address = Printf.sprintf "http://%s:%d%s" host port path in
           (* Update our mapping with the new address *)
           Hashtbl.replace loc_to_address loc_cfg.Config_parser.location new_address;
           (* Start server and return a promise that resolves when server is ready *)
           let promise, resolver = Lwt.task () in
           Lwt.async (fun () ->
             Lwt.catch
               (fun () ->
                  let callback _conn _req body =
                    let* body_string = Cohttp_lwt.Body.to_string body in
                    if String.length body_string > 0
                    then (
                      (* Try to unmarshal and print the data for debugging *)
                      (try
                         let data = Marshal.from_string body_string 0 in
                         Printf.printf
                           "DATA RECEIVED at %s: %s\n"
                           loc_cfg.Config_parser.location
                           (match data with
                            | _ when Obj.is_int (Obj.repr data) ->
                              "INT: " ^ string_of_int (Obj.magic data)
                            | _ when Obj.tag (Obj.repr data) = Obj.string_tag ->
                              "STRING: " ^ Obj.magic data
                            | _ ->
                              (try
                                 if
                                   Obj.is_block (Obj.repr data)
                                   && Obj.tag (Obj.repr data) = 0
                                 then (
                                   match Obj.magic data with
                                   | Ok v ->
                                     if Obj.is_int (Obj.repr v)
                                     then "Ok(" ^ string_of_int (Obj.magic v) ^ ")"
                                     else if Obj.tag (Obj.repr v) = Obj.string_tag
                                     then "Ok(\"" ^ Obj.magic v ^ "\")"
                                     else "Ok(<value>)"
                                   | Error msg -> "Error(\"" ^ msg ^ "\")")
                                 else "<complex data>"
                               with
                               | _ -> "<unprintable data>"));
                         flush stdout
                       with
                       | _ -> ());
                      (* Store message in queue *)
                      Hashtbl.replace
                        message_queues
                        loc_cfg.Config_parser.location
                        body_string;
                      (* Signal waiting receivers *)
                      Lwt_condition.broadcast message_condition ();
                      (* Respond with success *)
                      let resp_body = Cohttp_lwt.Body.of_string "" in
                      Lwt.return (Response.make ~status:`OK (), resp_body))
                    else (
                      (* Empty body means this is a polling request, not actual data *)
                      let data =
                        try
                          Some
                            (Hashtbl.find message_queues loc_cfg.Config_parser.location)
                        with
                        | Not_found -> None
                      in
                      match data with
                      | Some message ->
                        (* Return the message and remove from queue *)
                        Hashtbl.remove message_queues loc_cfg.Config_parser.location;
                        let resp_body = Cohttp_lwt.Body.of_string message in
                        Lwt.return (Response.make ~status:`OK (), resp_body)
                      | None ->
                        (* No message available *)
                        let resp_body = Cohttp_lwt.Body.of_string "" in
                        Lwt.return (Response.make ~status:`OK (), resp_body))
                  in
                  let server =
                    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
                  in
                  (* Once we've started the server, resolve the promise *)
                  Lwt.wakeup resolver ();
                  (* Return the server promise *)
                  server)
               (fun e ->
                  (* Only resolve if promise is still in Sleep state *)
                  if Lwt.state promise = Lwt.Sleep then Lwt.wakeup_exn resolver e;
                  Lwt.return_unit));
           (* Return the promise *)
           promise)
        locations_to_serve
    in
    (* Wait for all servers to be ready *)
    (try
       Lwt_main.run (Lwt.join server_promises);
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
     with
     | _ -> ())
;;

(** Function to receive a message; [receive_message ~location] is a promise that resolves to 
[Ok data] containing the received message data if a message is available for [location], and 
[Error msg] if receiving fails.
    
    The function first checks the local message queue. If no message is found,
    it polls the HTTP server for [location] repeatedly until a message arrives.
    This is a blocking operation that will retry indefinitely.
    
    Returns [Error msg] when:
    - Configuration is not initialized
    - [location] is unknown or not configured
    - Unmarshaling the received data fails
    - HTTP polling returns a non-200 status code
    
    Requires: 
    - Configuration must be initialized
    - [location] must exist in the configuration*)
let rec receive_message ~location =
  let actual_location = map_receive_location location in
  (* First check if we have a local message *)
  let local_message = Hashtbl.find_opt message_queues actual_location in
  match local_message with
  | Some data when String.length data > 0 ->
    (* Message found in queue, remove it *)
    Hashtbl.remove message_queues actual_location;
    (* Try to unmarshal the data *)
    (try
       let unmarshaled_data = Marshal.from_string data 0 in
       (* Print the actual unmarshaled data in readable format *)
       Printf.printf
         "DATA RECEIVED FROM QUEUE: %s\n"
         (match unmarshaled_data with
          | _ when Obj.is_int (Obj.repr unmarshaled_data) ->
            "INT: " ^ string_of_int (Obj.magic unmarshaled_data)
          | _ when Obj.tag (Obj.repr unmarshaled_data) = Obj.string_tag ->
            "STRING: " ^ Obj.magic unmarshaled_data
          | _ ->
            (try
               if
                 Obj.is_block (Obj.repr unmarshaled_data)
                 && Obj.tag (Obj.repr unmarshaled_data) = 0
               then (
                 match Obj.magic unmarshaled_data with
                 | Ok v ->
                   if Obj.is_int (Obj.repr v)
                   then "Ok(" ^ string_of_int (Obj.magic v) ^ ")"
                   else if Obj.tag (Obj.repr v) = Obj.string_tag
                   then "Ok(\"" ^ Obj.magic v ^ "\")"
                   else "Ok(<value>)"
                 | Error msg -> "Error(\"" ^ msg ^ "\")")
               else "<complex data>"
             with
             | _ -> "<unprintable data>"));
       flush stdout;
       Lwt.return_ok unmarshaled_data
     with
     | e -> Lwt.return_error ("Unmarshal error: " ^ Printexc.to_string e))
  | _ ->
    (* No local message, poll the remote server *)
    (match get_location_config actual_location with
     | Error msg -> Lwt.return_error msg
     | Ok loc_config ->
       Printf.printf
         "Polling remote server for location %s at %s\n"
         actual_location
         loc_config.http_address;
       flush stdout;
       Lwt.catch
         (fun () ->
            (* Make HTTP request to poll for messages *)
            let headers = Header.init_with "Content-Type" "application/octet-stream" in
            let body = Cohttp_lwt.Body.of_string "" in
            (* Empty body indicates polling *)
            Client.post ~headers ~body (Uri.of_string loc_config.http_address)
            >>= fun (resp, body) ->
            let status = resp |> Response.status |> Code.code_of_status in
            if status <> 200
            then
              Cohttp_lwt.Body.drain_body body
              >>= fun () ->
              Lwt.return_error
                ("Failed to poll for message, status code: " ^ string_of_int status)
            else
              Cohttp_lwt.Body.to_string body
              >>= fun body_str ->
              if String.length body_str = 0
              then
                (* No message available, wait a bit and try again *)
                let* () = Lwt_unix.sleep 0.5 in
                receive_message ~location
              else (
                (* Try to unmarshal the received data *)
                match unmarshal_data body_str with
                | Ok unmarshaled_data ->
                  Printf.printf
                    "DATA RECEIVED FROM HTTP: %s\n"
                    (match unmarshaled_data with
                     | _ when Obj.is_int (Obj.repr unmarshaled_data) ->
                       "INT: " ^ string_of_int (Obj.magic unmarshaled_data)
                     | _ when Obj.tag (Obj.repr unmarshaled_data) = Obj.string_tag ->
                       "STRING: " ^ Obj.magic unmarshaled_data
                     | _ -> "<complex data>");
                  flush stdout;
                  Lwt.return_ok unmarshaled_data
                | Error err -> Lwt.return_error err))
         (fun e ->
            Printf.printf "Error polling for message: %s\n" (Printexc.to_string e);
            flush stdout;
            (* Wait a bit and try again *)
            let* () = Lwt_unix.sleep 1.0 in
            receive_message ~location))
;;
