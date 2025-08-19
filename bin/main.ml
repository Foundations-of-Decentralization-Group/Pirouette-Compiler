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
  (match !ast_dump_format with
   | "json" -> Ast_utils.jsonify_choreo_ast (open_out (spf "%s.json" !basename)) program
   | "pprint" -> Ast_utils.pprint_choreo_ast (open_out (spf "%s.ast" !basename)) program
   | _ -> invalid_arg "Invalid ast-dump format");
  (* let location_holder = [] in *)
  (* let result_locations = Ast_utils.tree_walk program location_holder in *)
  (* List.iter (fun x -> Printf.printf "Location String : %s\n" x) result_locations; *)
  (* let result_set = Set.of_list result_locations in *)
  (* let result_locations_two = Set.to_list result_set in *)
  (* List.iter (fun x -> Printf.printf "Set location string : %s\n" x) result_locations_two; *)

  (* let copy_tree = Ast_utils.infer_sync_expr program result_locations_two in *)
  (* let program = copy_tree in *)
  (* let result_locations_new = get_stmt_block copy_tree location_holder_new in *)
  (* List.iter (fun x -> Printf.printf "Location String : %s\n" x) result_locations_new; *)
  let holder1 = [] in
  let holder2 = [] in
  let holder_3 = Ast_utils.optimize_sync_expr program holder1 holder2 in
  List.iter (fun x -> Printf.printf "Strings : %s\n" x) holder_3;
  let locs = Ast_utils.extract_locs program in
  let net_stmtblocks = List.map (fun loc -> Netgen.epp_choreo_to_net program loc) locs in
  List.iter2
    (fun loc stmtblock ->
       match !ast_dump_format with
       | "json" ->
         Ast_utils.jsonify_net_ast (open_out (spf "%s.%s.json" !basename loc)) stmtblock
       | "pprint" ->
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
