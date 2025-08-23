module Choreo = Ast_core.Choreo.M
module Local = Ast_core.Local.M
module Set = Set.Make (String)

let spf = Printf.sprintf
let usage_msg = "USAGE: pirc [options] <file>"
let ast_dump_format = ref "pprint"
let msg_backend = ref "domain"
let file_ic = ref None
let basename = ref ""

let anon_fun filename =
  basename := Filename.remove_extension (Filename.basename filename);
  file_ic := Some (open_in filename)
;;

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-ast-dump"
    , Arg.Symbol ([ "pprint"; "json" ], fun s -> ast_dump_format := s)
    , "Dump the AST in the specified format" )
  ; ( "-msg-backend"
    , Arg.Symbol ([ "domain"; "mpi" ], fun s -> msg_backend := s)
    , "Specify the backend for parallel execution" )
  ]
;;

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": no input file");
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.Parse.parse_with_error lexbuf in
  (* (match !ast_dump_format with *)
  (*  | "json" -> Ast_utils.jsonify_choreo_ast (open_out (spf "%s.json" !basename)) program *)
  (*  | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (spf "%s.ast" !basename)) program *)
  (*  | _ -> invalid_arg "Invalid ast-dump format"); *)
  let holder1 = [] in
  let holder2 = [] in
  let holder_3 = Ast_utils.optimize_sync_expr program holder1 holder2 in
  (* let size = List.length holder_3 in Printf.printf "This is the size of the list %d" size; *)
  (* List.iter (fun x -> Printf.printf "String to be used here : %s\n" x) holder_3; *)
  let optimization_flag =
    let optimization_flag_to_convert = List.hd holder_3 in
    Stdlib.bool_of_string optimization_flag_to_convert
  in
  let feed_list =
    if optimization_flag
    then (
      (* This is where we would do the optimization to reorder the control messages ; ie the call to appropriate function *)
      (* Have to do some list processing here to generate the right sends and receives for Sync messages *)
      print_endline "Optimization is possible";
      let rec remove_duplicates_of_head input_list value_of_head =
        match input_list with
        | [] -> []
        | head :: tail ->
          if String.equal head value_of_head
          then remove_duplicates_of_head tail value_of_head
          else head :: remove_duplicates_of_head tail value_of_head
      in
      let holder_3 = List.tl holder_3 in
      let head_val = List.hd holder_3 in
      let final_list =
        let result_list = remove_duplicates_of_head (List.tl holder_3) head_val in
        List.cons head_val result_list
      in
      let check_valid_index index length_of_list =
        if (index * 2) + 2 <= length_of_list - 1
        then "First and Second"
        else if (index * 2) + 1 = length_of_list - 1
        then "First"
        else "Nothing"
      in
      let rec print_order input_list index1 return_list =
        let match_value = check_valid_index index1 (List.length input_list) in
        match match_value with
        | "First and Second" ->
          let sender = List.nth input_list index1 in
          let recv_one = List.nth input_list ((index1 * 2) + 1) in
          let recv_two = List.nth input_list ((index1 * 2) + 2) in
          let result_list = List.cons sender return_list in
          let result_list = List.cons recv_one result_list in
          let result_list = List.cons sender result_list in
          let result_list = List.cons recv_two result_list in
          Printf.printf "%s -> %s \n %s -> %s \n" sender recv_one sender recv_two;
          print_order input_list (index1 + 1) result_list
        | "First" ->
          let sender = List.nth input_list index1 in
          let recv_one = List.nth input_list ((index1 * 2) + 1) in
          let result_list = List.cons sender return_list in
          let result_list = List.cons recv_one result_list in
          Printf.printf "%s -> %s \n" sender recv_one;
          print_endline "Done generating";
          result_list
        | _ ->
          print_endline "Done generating";
          return_list
      in
      print_order final_list 0 [])
    else (
      print_endline "Cannot do optimization";
      [])
  in
  let feed_list = List.rev feed_list in
  List.iter (fun x -> Printf.printf "Order of messages : %s\n" x) feed_list;
  let program_one = Ast_utils.add_sync_opt program feed_list in
  (match !ast_dump_format with
   | "json" -> Ast_utils.jsonify_choreo_ast (open_out (spf "%s.json" !basename)) program_one
   | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (spf "%s.ast" !basename)) program_one
   | _ -> invalid_arg "Invalid ast-dump format");
  let locs = Ast_utils.extract_locs program_one in
  List.iter (fun x -> Printf.printf "These are the locations : %s\n" x) locs;
  let net_stmtblocks =
    List.map (fun loc -> Netgen.epp_choreo_to_net program_one loc) locs
  in
  List.iter2
    (fun loc stmtblock ->
       match !ast_dump_format with
       | "json" ->
         Ast_utils.jsonify_net_ast (open_out (spf "%s.%s.json" !basename loc)) stmtblock
       | "pprint" ->
         print_endline "Count";
         Ast_utils.pprint_net_ast (open_out (spf "%s.%s.ast" !basename loc)) stmtblock
       | _ -> invalid_arg "Invalid ast-dump format")
    locs
    net_stmtblocks;
  match !msg_backend with
  | "domain" ->
    Ocamlgen.Toplevel_domain.emit_toplevel_domain
      (open_out (spf "%s.domain.ml" !basename))
      locs
      net_stmtblocks
  | "mpi" ->
    Ocamlgen.Toplevel_mpi.emit_toplevel_mpi
      (open_out (spf "%s.mpi.ml" !basename))
      locs
      net_stmtblocks
  | _ -> invalid_arg "Invalid backend"
;;
