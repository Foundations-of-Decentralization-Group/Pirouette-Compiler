let _space: string = " "
let _unit: string = "()"
let _sndmsg: string = "Evt.send"
let _lParen: string = "("
let _rParen: string = ")"
let _endl: string = "\n"
let _rcvmsg: string = "Evt.receive"

let _sync: string = "Evt.sync"
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
let _disreg: string = "__disreg"
let _true: string = "true"
let _false: string = "false"
let _underscore: string = "_"
let _quotify (arg: string) : string = "\"" ^ arg ^ "\""
let _colon: string = ":"
let _gt: string = ">"
let _lt: string = "<"

let _rec: string = "rec"
let _utilImport: string = "open Unix

let open_port port =
  let server_sock = socket PF_INET SOCK_DGRAM 0 in
  let server_addr = ADDR_INET (inet_addr_any, port) in
  bind server_sock server_addr;
  server_sock

let receive_message server_sock =
  let buffer_size = 1024 in
  let buffer = Bytes.create buffer_size in
  let received_bytes, _ = recvfrom server_sock buffer 0 buffer_size [] in
  let message = Bytes.sub_string buffer 0 received_bytes in
  message

let send_message message loc =
  let sock = socket PF_INET SOCK_DGRAM 0 in
  let server_addr = ADDR_INET (inet_addr_of_string \"127.0.0.1\", loc) in
  let _ = sendto sock (Bytes.of_string message) 0 (String.length message) [] server_addr in
  close sock"

let _threadImport = "module Evt = Event \n \n"
let _commonBp: string =  _let ^ _space ^ _unit ^ _space ^ _equals ^ _space
let _boilerPlate: string = _utilImport ^ _endl ^ _endl ^ _endl ^ _commonBp
let _serverBoilerPlate port: string = "let __server_sock = open_port " ^ port ^ " in \n"
(* Add more constants as needed *)