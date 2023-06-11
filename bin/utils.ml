open Unix

let receive_message client_sock =
  let buffer_size = 1024 in
  let buffer = Bytes.create buffer_size in
  let received_bytes = recv client_sock buffer 0 buffer_size [] in
  let message = Bytes.sub_string buffer 0 received_bytes in
  message

let listen_and_accept port =
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
  send_message message *)