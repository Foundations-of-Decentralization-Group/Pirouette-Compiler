let _space: string = " "
let _unit: string = "()"
let _sndmsg: string = "send_message"
let _lParen: string = "("
let _rParen: string = ")"
let _endl: string = "\n"
let _rcvmsg: string = "receive_message"
let _let: string = "let"
let _in: string = "in"
let _tab: string = "\t"
let _equals: string = " = "
let _if: string = "if"
let _then: string = "then"
let _else: string = "else"
let _lSqParen: string = "["
let _rSqParen: string = "]"
let _plus: string = "+"
let _minus: string = "-"
let _product: string = "*"
let _division: string = "/"
let _disreg: string = "___disreg"
let _true: string = "true"
let _false: string = "false"
let _underscore: string = "_"
let _quotify (arg: string) : string = "\"" ^ arg ^ "\""
let _utilImport: string = "open Unix

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
  let server_addr = ADDR_INET (inet_addr_of_string \"127.0.0.1\", loc) in
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
      | _ -> failwith (\"Invalid line: \" ^ line)
    done;
    config_table
  with
  | End_of_file ->
      close_in in_channel;
      config_table
  | exn ->
      close_in_noerr in_channel;
      raise exn"
let _boilerPlate: string = _utilImport ^ _endl ^ _endl ^ _endl ^ _let ^ _space ^ _unit ^ _space ^ _equals ^ _space
let _serverBoilerPlate port: string = "let ___server_sock = open_port " ^ port ^ " in \n"
(* Add more constants as needed *)