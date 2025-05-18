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

let rec matcher_local_expr (x : 'a Local.expr) =
  match x with
  | Unit _ -> print_endline "Hit a Local Unit AST Node"
  | Val (v, _) ->
    print_endline "Hit a Local Val Ast Node";
    matcher_local_val v
  | Var (v, _) ->
    print_endline "Hit a Local Var Ast Node";
    match_loc_var_id v
  | UnOp (u, e, _) ->
    print_endline "Hit a Local Unop Ast Node";
    matcher_local_unop u;
    matcher_local_expr e
  | BinOp (e1, bop, e2, _) ->
    print_endline "Hit a Local Binop Node";
    matcher_local_expr e1;
    matcher_local_binop bop;
    matcher_local_expr e2
  | Let (v, e1, e2, _) ->
    print_endline "Hit a Local Let AST node";
    match_loc_var_id v;
    matcher_local_expr e1;
    matcher_local_expr e2
  | Pair (e1, e2, _) ->
    print_endline "Hit a Local Pair AST node";
    matcher_local_expr e1;
    matcher_local_expr e2
  | Fst (e, _) ->
    print_endline "Hit a Local Fst AST node";
    matcher_local_expr e
  | Snd (e, _) ->
    print_endline "Hit a Local Snd AST node";
    matcher_local_expr e
  | Left (e, _) ->
    print_endline "Hit a Local Left AST node";
    matcher_local_expr e
  | Right (e, _) ->
    print_endline "Hit a Local Right AST node";
    matcher_local_expr e
  | Match (e, l, _) ->
    print_endline "Hit a Local Match AST node";
    matcher_local_expr e;
    matcher_local_case l

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

and matcher_local_case (x : ('a Local.pattern * 'a Local.expr) list) =
  match x with
  | [] -> print_endline "Done with the list of cases"
  | head :: tail ->
    split_local_pat_expr head;
    matcher_local_case tail

and split_local_pat_expr (x : 'a Local.pattern * 'a Local.expr) =
  match x with
  | p, e ->
    match_loc_pattern p;
    matcher_local_expr e

and match_loc_pattern (x : 'a Local.pattern) =
  match x with
  | Default _ -> print_endline "Hit a local Default pattern AST node"
  | Val (v, _) ->
    print_endline "Hit a local Val pattern AST node";
    matcher_local_val v
  | Var (v, _) ->
    print_endline "Hit a local Var pattern AST node";
    match_loc_var_id v
  | Pair (p1, p2, _) ->
    print_endline "Hit a local Pair pattern AST node";
    match_loc_pattern p1;
    match_loc_pattern p2
  | Left (p, _) ->
    print_endline "Hit a local Left pattern AST node";
    match_loc_pattern p
  | Right (p, _) ->
    print_endline "Hit a local Right pattern AST node";
    match_loc_pattern p
;;

let rec get_stmt_block (p : 'a Choreo.stmt list) =
  match p with
  | [] -> print_endline "Done with the statement list"
  | head :: tail ->
    matcher_stmt head;
    get_stmt_block tail

and matcher_stmt (x : 'a Choreo.stmt) =
  match x with
  | Decl (p, t, _) ->
    print_endline "Hit the Decl AST node";
    matcher_pattern p;
    matcher_choreo_typ t
  | Assign (ps, e, _) ->
    print_endline "Hit the Assign AST node";
    get_pattern_list ps;
    matcher_choreo_expr e
  | TypeDecl (t1, t2, _) ->
    print_endline "Hit the TypeDecl AST node";
    matcher_loc_typ_id t1;
    matcher_choreo_typ t2

and matcher_choreo_expr (x : 'a Choreo.expr) =
  match x with
  | Unit _ -> print_endline "Hit the Unit AST node in Choreo"
  | Var _ -> print_endline "Hit the Var AST node in Choreo"
  | LocExpr (l, e, _) ->
    Printf.printf "Hit the LocExpr AST node\n";
    matcher_loc_id l;
    matcher_local_expr e
  | Send (l1, e, l2, _) ->
    Printf.printf "Hit the Send Ast node\n";
    matcher_loc_id l1;
    matcher_choreo_expr e;
    matcher_loc_id l2
  | Sync (l1, l2, l3, e, _) ->
    Printf.printf "Hit the Sync Ast node";
    matcher_loc_id l1;
    matcher_sync_label_id l2;
    matcher_loc_id l3;
    matcher_choreo_expr e
  | If (e1, e2, e3, _) ->
    print_endline "Hit the If AST node";
    matcher_choreo_expr e1;
    matcher_choreo_expr e2;
    matcher_choreo_expr e3
  | Let (stmts, e, _) ->
    print_endline "Hit the Let AST node";
    get_stmt_block stmts;
    matcher_choreo_expr e
  | FunDef (ps, e, _) ->
    print_endline "Hit the FunDef AST node";
    get_pattern_list ps;
    matcher_choreo_expr e
  | FunApp (e1, e2, _) ->
    print_endline "Hit the FunApp AST node";
    matcher_choreo_expr e1;
    matcher_choreo_expr e2
  | Pair (e1, e2, _) ->
    print_endline "Hit the Pair AST node";
    matcher_choreo_expr e1;
    matcher_choreo_expr e2
  | Fst (e, _) ->
    print_endline "Hit the FST AST node";
    matcher_choreo_expr e
  | Snd (e, _) ->
    print_endline "Hit the Snd AST node";
    matcher_choreo_expr e
  | Left (e, _) ->
    print_endline "Hit the Left AST node";
    matcher_choreo_expr e
  | Right (e, _) ->
    print_endline "Hit the Right AST node";
    matcher_choreo_expr e
  | Match (e, cases, _) ->
    print_endline "Hit the Match AST node";
    matcher_choreo_expr e;
    matcher_choreo_case cases

and matcher_choreo_typ (x : 'a Choreo.typ) =
  match x with
  | TUnit _ -> print_endline "Hit the Choreo typ's TUnit AST node"
  | TLoc (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TLoc AST node";
    matcher_loc_id t1;
    matcher_local_typ t2
  | TMap (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TMap AST node";
    matcher_choreo_typ t1;
    matcher_choreo_typ t2
  | TProd (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TProd AST node";
    matcher_choreo_typ t1;
    matcher_choreo_typ t2
  | TSum (t1, t2, _) ->
    print_endline "Hit the Choreo typ's TSum AST node";
    matcher_choreo_typ t1;
    matcher_choreo_typ t2

and matcher_local_typ (x : 'a Local.typ) =
  match x with
  | TUnit _ -> print_endline "Hit the Local typ's TUnit AST node"
  | TInt _ -> print_endline "Hit the Local typ's TInt AST node"
  | TString _ -> print_endline "Hit the Local typ's TString AST node"
  | TBool _ -> print_endline "Hit the Local typ's TBool AST node"
  | TProd (t1, t2, _) ->
    print_endline "Hit the Local typ's TProd AST node";
    matcher_local_typ t1;
    matcher_local_typ t2
  | TSum (t1, t2, _) ->
    print_endline "Hit the Local typ's TSum AST node";
    matcher_local_typ t1;
    matcher_local_typ t2

and matcher_loc_id (x : 'a Local.loc_id) =
  match x with
  | LocId (_, _) -> print_endline "Hit the Loc Id AST node"

and matcher_loc_typ_id (x : 'a Local.typ_id) =
  match x with
  | TypId (_, _) -> print_endline "Hit the Typ ID Ast node"

and matcher_sync_label_id (x : 'a Local.sync_label) =
  match x with
  | LabelId (_, _) -> print_endline "Hit the Sync Label Id AST node"

and matcher_choreo_case (x : ('a Choreo.pattern * 'a Choreo.expr) list) =
  match x with
  | [] -> print_endline "Done with the list of cases"
  | head :: tail ->
    split_choreo_pat_expr head;
    matcher_choreo_case tail

and split_choreo_pat_expr (x : 'a Choreo.pattern * 'a Choreo.expr) =
  match x with
  | p, e ->
    matcher_pattern p;
    matcher_choreo_expr e

and matcher_pattern (x : 'a Choreo.pattern) =
  match x with
  | Default _ -> print_endline "Hit a Default AST node"
  | Var _ -> print_endline "Hit a Var AST node"
  | Pair (p1, p2, _) ->
    print_endline "Hit a Pair AST node";
    matcher_pattern p1;
    matcher_pattern p2
  | LocPat (l1, l2, _) ->
    Printf.printf "Hit a LocalPattern AST node\n";
    matcher_loc_id l1;
    match_loc_pattern l2
  | Left (p, _) ->
    print_endline "Hit a Left AST node";
    matcher_pattern p
  | Right (p, _) ->
    print_endline "Hit a Right AST node";
    matcher_pattern p

and get_pattern_list (p : 'a Choreo.pattern list) =
  match p with
  | [] -> print_endline "Done with the pattern list"
  | head :: tail ->
    matcher_pattern head;
    get_pattern_list tail
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
  get_stmt_block program;
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
