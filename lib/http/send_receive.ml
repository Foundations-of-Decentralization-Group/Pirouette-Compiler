open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

(* Global config reference *)
let config = ref None

(* Initialize config from file *)
let init () =
  let config_filename = "test/example.yaml" in
  Printf.printf "Loading config from: %s\n" config_filename;
  Config_parser.load_config config_filename >>= function
  | Some cfg -> 
      config := Some cfg; 
      Lwt.return_ok ()
  | None -> 
      Lwt.return_error ("Failed to load config from " ^ config_filename)

(* Helper to get location config *)
let get_location_config location =
  match !config with
  | None -> Error "Config not initialized. Call init() first"
  | Some cfg ->
    match List.find_opt (fun loc -> loc.Config_parser.location = location) cfg.Config_parser.locations with
    | Some loc_config -> Ok loc_config
    | None -> Error ("Unknown location: " ^ location)

(* Function to marshal data *)
let marshal_data data = Marshal.to_string data []

(* Function to unmarshal data *)
let unmarshal_data data_str =
  try Ok (Marshal.from_string data_str 0) with
  | Failure msg -> Error ("Unmarshal error: " ^ msg)
;;

(* Function to send a message *)
let send_message ~location ~data =
  match get_location_config location with
  | Error msg -> Lwt.return_error msg
  | Ok loc_config ->
    let marshaled_data = marshal_data data in
    let headers = Header.init_with "Content-Type" "application/octet-stream" in
    let body = Cohttp_lwt.Body.of_string marshaled_data in
    Client.post ~headers ~body (Uri.of_string loc_config.http_address)
    >>= fun (resp, body) ->
    let status = resp |> Response.status |> Code.code_of_status in
    Cohttp_lwt.Body.drain_body body
    >>= fun () ->
    if status = 200
    then Lwt.return_ok ()
    else Lwt.return_error ("Failed to send message, status code: " ^ string_of_int status)

(* Function to receive a message *)
let receive_message ~location =
  match get_location_config location with
  | Error msg -> Lwt.return_error msg
  | Ok loc_config ->
    Client.get (Uri.of_string loc_config.http_address)
    >>= fun (resp, body) ->
    let status = resp |> Response.status |> Code.code_of_status in
    if status = 200
    then
      body
      |> Cohttp_lwt.Body.to_string
      >|= fun body_str ->
      match unmarshal_data body_str with
      | Ok data -> Ok data
      | Error msg -> Error msg
    else
      Cohttp_lwt.Body.drain_body body
      >>= fun () ->
      Lwt.return_error ("Failed to receive message, status code: " ^ string_of_int status)
