module Choreo = Ast_core.Choreo.M
module Local = Ast_core.Local.M

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

let rec matcher_local_expr (x : 'a Local.expr) (holder : string list) : string list =
  match x with
  | Unit _ ->
    print_endline "Hit a Local Unit AST Node";
    holder
  | Val (v, _) ->
    print_endline "Hit a Local Val Ast Node";
    matcher_local_val v;
    holder
  | Var (v, _) ->
    print_endline "Hit a Local Var Ast Node";
    match_loc_var_id v;
    holder
  | UnOp (u, e, _) ->
    print_endline "Hit a Local Unop Ast Node";
    matcher_local_unop u;
    matcher_local_expr e holder
  | BinOp (e1, bop, e2, _) ->
    print_endline "Hit a Local Binop Node";
    matcher_local_binop bop;
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Let (v, e1, e2, _) ->
    print_endline "Hit a Local Let AST node";
    match_loc_var_id v;
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Pair (e1, e2, _) ->
    print_endline "Hit a Local Pair AST node";
    let holder = matcher_local_expr e1 holder in
    matcher_local_expr e2 holder
  | Fst (e, _) ->
    print_endline "Hit a Local Fst AST node";
    matcher_local_expr e holder
  | Snd (e, _) ->
    print_endline "Hit a Local Snd AST node";
    matcher_local_expr e holder
  | Left (e, _) ->
    print_endline "Hit a Local Left AST node";
    matcher_local_expr e holder
  | Right (e, _) ->
    print_endline "Hit a Local Right AST node";
    matcher_local_expr e holder
  | Match (e, l, _) ->
    print_endline "Hit a Local Match AST node";
    let holder = matcher_local_expr e holder in
    matcher_local_case l holder

and matcher_local_val (x : 'a Local.value) =
  match x with
  | Int (_, _) -> print_endline "Hit a Local Int AST node"
  | String (_, _) -> print_endline "Hit a Local String AST node"
  | Bool (_, _) -> print_endline "Encountered a Local Bool AST node"

and match_loc_var_id (x : 'a Local.var_id) =
  match x with
  | VarId (_, _) -> print_endline "Hit a local Var ID AST node"

and matcher_local_unop (x : 'a Local.un_op) =
  match x with
  | Not _ -> print_endline "Hit a local Not Unop AST node"
  | Neg _ -> print_endline "Hit a local Neg Unop AST node"

and matcher_local_binop (x : 'a Local.bin_op) =
  match x with
  | Plus _ -> print_endline "Hit a Local Binop Node of type plus"
  | Minus _ -> print_endline "Hit a Local Binop Node of type minus"
  | Times _ -> print_endline "Hit a Local Binop Node of type times"
  | Div _ -> print_endline "Hit a Local Binop Node of type division"
  | And _ -> print_endline "Hit a Local Binop Node of type and"
  | Or _ -> print_endline "Hit a Local Binop Node of type or"
  | Eq _ -> print_endline "Hit a Local Binop Node of type eq"
  | Neq _ -> print_endline "Hit a Local Binop Node of type Neq"
  | Lt _ -> print_endline "Hit a Local Binop Node of type Lt"
  | Leq _ -> print_endline "Hit a Local Binop Node of type Leq"
  | Gt _ -> print_endline "Hit a Local Binop Node of type Gt"
  | Geq _ -> print_endline "Hit a Local Binop Node of type Geq"

and matcher_local_case
      (x : ('a Local.pattern * 'a Local.expr) list)
      (holder : string list)
  : string list
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    holder
  | head :: tail ->
    let holder = split_local_pat_expr head holder in
    matcher_local_case tail holder

and split_local_pat_expr (x : 'a Local.pattern * 'a Local.expr) (holder : string list)
  : string list
  =
  match x with
  | p, e ->
    let holder = match_loc_pattern p holder in
    matcher_local_expr e holder

and match_loc_pattern (x : 'a Local.pattern) (holder : string list) : string list =
  match x with
  | Default _ ->
    print_endline "Hit a local Default pattern AST node";
    holder
  | Val (v, _) ->
    print_endline "Hit a local Val pattern AST node";
    matcher_local_val v;
    holder
  | Var (v, _) ->
    print_endline "Hit a local Var pattern AST node";
    match_loc_var_id v;
    holder
  | Pair (p1, p2, _) ->
    print_endline "Hit a local Pair pattern AST node";
    let holder = match_loc_pattern p1 holder in
    match_loc_pattern p2 holder
  | Left (p, _) ->
    print_endline "Hit a local Left pattern AST node";
    match_loc_pattern p holder
  | Right (p, _) ->
    print_endline "Hit a local Right pattern AST node";
    match_loc_pattern p holder
;;

let rec get_stmt_block (p : 'a Choreo.stmt list) (holder : string list) : string list =
  match p with
  | [] ->
    print_endline "Done with the statement list";
    holder
  | head :: tail ->
    let holder = matcher_stmt head holder in
    get_stmt_block tail holder

and matcher_stmt (x : 'a Choreo.stmt) (holder : string list) : string list =
  match x with
  | Decl (p, t, _) ->
    print_endline "Hit the Decl AST node";
    let holder1 = matcher_pattern p holder in
    matcher_choreo_typ t holder1
  | Assign (ps, e, _) ->
    print_endline "Hit the Assign AST node";
    let holder1 = get_pattern_list ps holder in
    matcher_choreo_expr e holder1
  | TypeDecl (t1, t2, _) ->
    print_endline "Hit the TypeDecl AST node";
    let holder1 = matcher_loc_typ_id t1 holder in
    matcher_choreo_typ t2 holder1

and matcher_choreo_expr (x : 'a Choreo.expr) (holder : string list) : string list =
  match x with
  | Unit _ ->
    print_endline "Hit the Unit AST node in Choreo";
    holder
  | Var _ ->
    print_endline "Hit the Var AST node in Choreo";
    holder
  | LocExpr (l, e, _) ->
    print_endline "Hit the LocExpr AST node\n";
    let holder = matcher_loc_id l holder in
    matcher_local_expr e holder
  | Send (l1, e, l2, _) ->
    print_endline "Hit the Send Ast node\n";
    let final_holder =
      let holder = matcher_loc_id l1 holder in
      matcher_choreo_expr e holder
    in
    matcher_loc_id l2 final_holder
    (* matcher_loc_id l2 holder *)
  | Sync (l1, l2, l3, e, _) ->
    print_endline "Hit the Sync Ast node";
    matcher_sync_label_id l2;
    let holder2 =
      let holder1 = matcher_loc_id l1 holder in
      matcher_loc_id l3 holder1
    in
    matcher_choreo_expr e holder2
    (* matcher_choreo_expr e *)
  | If (e1, e2, e3, _) ->
    print_endline "Hit the If AST node";
    let holder2 =
      let holder1 = matcher_choreo_expr e1 holder in
      matcher_choreo_expr e2 holder1
    in
    matcher_choreo_expr e3 holder2
    (* matcher_choreo_expr e2; *)
    (* matcher_choreo_expr e3 *)
  | Let (stmts, e, _) ->
    print_endline "Hit the Let AST node";
    let holder1 = get_stmt_block stmts holder in
    matcher_choreo_expr e holder1
  | FunDef (ps, e, _) ->
    print_endline "Hit the FunDef AST node";
    let holder1 = get_pattern_list ps holder in
    matcher_choreo_expr e holder1
  | FunApp (e1, e2, _) ->
    print_endline "Hit the FunApp AST node";
    let holder = matcher_choreo_expr e1 holder in
    matcher_choreo_expr e2 holder
  | Pair (e1, e2, _) ->
    print_endline "Hit the Pair AST node";
    let holder = matcher_choreo_expr e1 holder in
    matcher_choreo_expr e2 holder
  | Fst (e, _) ->
    print_endline "Hit the FST AST node";
    matcher_choreo_expr e holder
  | Snd (e, _) ->
    print_endline "Hit the Snd AST node";
    matcher_choreo_expr e holder
  | Left (e, _) ->
    print_endline "Hit the Left AST node";
    matcher_choreo_expr e holder
  | Right (e, _) ->
    print_endline "Hit the Right AST node";
    matcher_choreo_expr e holder
  | Match (e, cases, _) ->
    print_endline "Hit the Match AST node";
    let holder1 = matcher_choreo_expr e holder in
    matcher_choreo_case cases holder1

and matcher_choreo_typ (x : 'a Choreo.typ) (holder : string list) : string list =
  match x with
  | TUnit _ ->
    print_endline "Hit the Choreo typ's TUnit AST node";
    holder
  | TLoc (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TLoc AST node";
    let holder = matcher_loc_id t1 holder in
    matcher_local_typ t2 holder
  | TMap (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TMap AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder
  | TProd (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TProd AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder
  | TSum (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TSum AST node";
    let holder = matcher_choreo_typ t1 holder in
    matcher_choreo_typ t2 holder

and matcher_local_typ (x : 'a Local.typ) (holder : string list) =
  match x with
  | TUnit _ ->
    print_endline "Hit the Local typ's TUnit AST node";
    holder
  | TInt _ ->
    print_endline "Hit the Local typ's TInt AST node";
    holder
  | TString _ ->
    print_endline "Hit the Local typ's TString AST node";
    holder
  | TBool _ ->
    print_endline "Hit the Local typ's TBool AST node";
    holder
  | TProd (t1, t2, _) ->
    print_endline "Hit the Local typ's TProd AST node";
    let holder1 = matcher_local_typ t1 holder in
    matcher_local_typ t2 holder1
  | TSum (t1, t2, _) ->
    print_endline "Hit the Local typ's TSum AST node";
    let holder1 = matcher_local_typ t1 holder in
    matcher_local_typ t2 holder1

and matcher_loc_id (x : 'a Local.loc_id) (holder : string list) : string list =
  match x with
  | LocId (id, _) ->
    print_endline "Hit the Loc Id AST node";
    Printf.printf "This is the location %s\n" id;
    let holder = List.cons id holder in
    holder

and matcher_loc_typ_id (x : 'a Local.typ_id) (holder : string list) =
  match x with
  | TypId (_, _) ->
    print_endline "Hit the Typ ID Ast node";
    holder

and matcher_sync_label_id (x : 'a Local.sync_label) =
  match x with
  | LabelId (_, _) -> print_endline "Hit the Sync Label Id AST node"

and matcher_choreo_case
      (x : ('a Choreo.pattern * 'a Choreo.expr) list)
      (holder : string list)
  =
  match x with
  | [] ->
    print_endline "Done with the list of cases";
    holder
  | head :: tail ->
    let holder1 = split_choreo_pat_expr head holder in
    matcher_choreo_case tail holder1

and split_choreo_pat_expr (x : 'a Choreo.pattern * 'a Choreo.expr) (holder : string list)
  : string list
  =
  match x with
  | p, e ->
    let holder1 = matcher_pattern p holder in
    matcher_choreo_expr e holder1

(* Need a good return from this guy *)
and matcher_pattern (x : 'a Choreo.pattern) (holder : string list) : string list =
  match x with
  | Default _ ->
    print_endline "Hit a Default AST node";
    holder
  | Var _ ->
    print_endline "Hit a Var AST node";
    holder
  | Pair (p1, p2, _) ->
    print_endline "Hit a Pair AST node";
    let holder = matcher_pattern p1 holder in
    matcher_pattern p2 holder
  | LocPat (l1, l2, _) ->
    Printf.printf "Hit a LocalPattern AST node\n";
    let holder = matcher_loc_id l1 holder in
    match_loc_pattern l2 holder
  | Left (p, _) ->
    print_endline "Hit a Left AST node";
    matcher_pattern p holder
  | Right (p, _) ->
    print_endline "Hit a Right AST node";
    matcher_pattern p holder

and get_pattern_list (p : 'a Choreo.pattern list) (holder : string list) =
  match p with
  | [] ->
    print_endline "Done with the pattern list";
    holder
  | head :: tail ->
    let holder1 = matcher_pattern head holder in
    get_pattern_list tail holder1
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
  let location_holder = [] in
  let result_locations = get_stmt_block program location_holder in
  List.iter (fun x -> Printf.printf "Location String : %s\n" x) result_locations;
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
