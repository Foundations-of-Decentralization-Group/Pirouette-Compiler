(* open Unix

let receive_message client_sock =
  let buffer_size = 1024 in
  let buffer = Bytes.create buffer_size in
  let received_bytes = recv client_sock buffer 0 buffer_size [] in
  let message = Bytes.sub_string buffer 0 received_bytes in
  message

let listen_and_accept (port: int) =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = ADDR_INET (inet_addr_any, port) in
  bind server_sock server_addr;
  listen server_sock 5;

  while true do
    let (client_sock, _) = accept server_sock in
    let received_message = receive_message client_sock in
    print_endline received_message;
    close client_sock;
  done;
  close server_sock


let send_message message loc =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string "127.0.0.1", loc) in
  connect sock server_addr;
  let _ = send sock (Bytes.of_string message) 0 (String.length message) [] in ()
  (* close sock *)

(* let () =
  let port = 8083 in
  listen_and_accept port *)

  (* let () =
  let message = "Hello, server!" in
  send_message message;;
  let message = "hello!" in
  send_message message *) *)

open Unix

let open_port port =
  let server_sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = ADDR_INET (inet_addr_any, port) in
  bind server_sock server_addr;
  listen server_sock 5;
  server_sock

let receive_message server_sock =
  let (client_sock, _) = accept server_sock in 
  let buffer_size = 1024 in
  let buffer = Bytes.create buffer_size in
  let received_bytes = recv client_sock buffer 0 buffer_size [] in
  let message = Bytes.sub_string buffer 0 received_bytes in
  int_of_string message

let send_message message loc =
  let sock = socket PF_INET SOCK_STREAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string "127.0.0.1", loc) in
  connect sock server_addr;
  let _ = send sock (Bytes.of_string message) 0 (String.length message) [] in
  close sock
  
let create_server_sockets input_hashmap =
  let output_hashmap = Hashtbl.create 10 in
  let process_entry name port =
    let server_sock = open_port port in
    Hashtbl.add output_hashmap name server_sock
  in
  Hashtbl.iter process_entry input_hashmap;
  output_hashmap

let read_config_file (filename : string) : (string, int) Hashtbl.t =
  let config_table = Hashtbl.create 10 in
  let in_channel = open_in filename in
  try
    while true do
      let line = input_line in_channel in
      match String.split_on_char '=' line with
      | [key; value] ->
          let key = String.trim key in
          let value = int_of_string (String.trim value) in
          Hashtbl.add config_table key value
      | _ -> failwith ("Invalid line: " ^ line)
    done;
    config_table
  with
  | End_of_file ->
      close_in in_channel;
      config_table
  | exn ->
      close_in_noerr in_channel;
      raise exn