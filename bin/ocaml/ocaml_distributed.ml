open Backend_intf
open Constants

module Ocaml_distributed : Comm = struct

  let exit (_entities : (string, int) Hashtbl.t) = ""

  let init (entities : (string, int) Hashtbl.t) (cur_entity: string) =
    let port = Hashtbl.find entities cur_entity in
    _let ^ " ___socket" ^ _space ^ _equals ^ _space ^ "open_port" ^ _space ^ (string_of_int port) ^ _space ^ _in

  let send to_loc _from_loc codified_arg (entities : (string, int) Hashtbl.t) = 
    let port = Hashtbl.find entities to_loc in
    _let ^ _space ^ "_" ^ _equals ^ _space ^ "send_message" ^ "(string_of_int " ^ codified_arg ^ ")" ^ (string_of_int port) 

  let rcv _to_loc _from_loc bndr bndr_type =  _let ^ _space ^ bndr ^ _colon ^ bndr_type ^ _equals ^ _space ^ 
  "receive_message" ^ _space ^ "___socket" 

end


