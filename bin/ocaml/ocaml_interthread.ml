open Backend_intf
open Constants

module Ocaml_interthread : Comm = struct
  
  let output_code_for_entity (entity : string) =
    let thread_code = "let " ^ _underscore ^ entity ^ " = Thread.create " ^ entity ^ " () in " in
    thread_code
  
  let hashtable_to_string_list tbl =
    let keys_list = ref [] in
    Hashtbl.iter (fun key _ -> keys_list := key :: !keys_list) tbl;
    !keys_list

  let exit (entities : (string, int) Hashtbl.t) =
    let entities_list = hashtable_to_string_list entities in 
    let code_strings = List.map output_code_for_entity entities_list in
    let joined_code = String.concat "\n" code_strings in
    let join_code = List.map (fun entity -> "Thread.join " ^ _underscore ^ entity ^ ";") entities_list in
    let joined_join_code = String.concat "\n" join_code in
    joined_code ^ "\n" ^ joined_join_code

    
  let output_code_for_pair (p1 : string) (p2 : string) =
    let channel_p1_p2 = "let _channel_" ^ p1 ^ "_" ^ p2 ^ " = Evt.new_channel () in " in
    let channel_p2_p1 = "let _channel_" ^ p2 ^ "_" ^ p1 ^ " = Evt.new_channel () in " in
    channel_p1_p2 ^ "\n" ^ channel_p2_p1

  let init (entities : (string, int) Hashtbl.t) (_ : string) =
    let buffer = Buffer.create 256 in
    let entities_list = hashtable_to_string_list entities in 
    let rec iterate_pairs = function
      | [] -> () (* Base case: empty list *)
      | [_] -> () (* Base case: only one entity left, no pair to create *)
      | p1 :: rest ->
        List.iter (fun p2 ->
          let code = output_code_for_pair p1 p2 in
          Buffer.add_string buffer code;
          Buffer.add_char buffer '\n') rest;
        iterate_pairs rest
    in
    iterate_pairs entities_list;
    Buffer.contents buffer

  let send to_loc from_loc codified_arg _ = _let ^ _space ^ "_" ^ _equals 
  ^ _space ^ _sync ^ _space ^  _lParen ^ _sndmsg ^ _space ^ 
  _space ^ "_channel_" ^ from_loc ^ _underscore ^ to_loc ^ _space ^ 
  codified_arg ^ _rParen

  let rcv to_loc from_loc bndr bndr_type =  _let ^ _space ^ bndr ^ _colon ^ bndr_type ^ _equals ^ _space ^ _sync ^ _space ^  
  _lParen ^ _rcvmsg ^ _space ^ "_channel_" ^ to_loc ^ _underscore ^ from_loc ^ 
   _space ^ _rParen 
end


