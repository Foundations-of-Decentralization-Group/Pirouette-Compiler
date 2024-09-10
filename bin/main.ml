let usage_msg = "USAGE: pirc <src_file> [-pprint] [-json] [-dot] [-o <output_file>]"
let pprint_flag = ref false
let json_flag = ref false
let dot_flag = ref false
let file_ic = ref None
let outfile_name = ref ""
let open_file_ic filename = file_ic := Some (open_in filename)

let speclist =
  [ "-", Arg.Unit (fun () -> file_ic := Some stdin), "Read source from stdin"
  ; ( "-pprint"
    , Arg.Set pprint_flag
    , "Pretty print the parsed program and Net IR, default option if no flag provided" )
  ; "-json", Arg.Set json_flag, "Output the AST in JSON format"
  ; "-dot", Arg.Set dot_flag, "Output the AST in DOT format"
  ; ( "-o"
    , Arg.Set_string outfile_name
    , "Specify the output file name (optional). If provided, the program will \
       automatically add the .pir/.json/.dot extension" )
  ]
;;

let () =
  Arg.parse speclist open_file_ic usage_msg;
  if !file_ic = None
  then (
    prerr_endline (Sys.argv.(0) ^ ": No input file");
    prerr_endline usage_msg;
    exit 1);
  let lexbuf = Lexing.from_channel (Option.get !file_ic) in
  let program = Parsing.parse_program lexbuf in
  if (not !pprint_flag) && (not !json_flag) && not !dot_flag then pprint_flag := true;
  if !outfile_name <> ""
  then (
    if !pprint_flag
    then Ast_utils.pprint_choreo_ast (open_out (!outfile_name ^ ".pir")) program;
    if !json_flag
    then Ast_utils.jsonify_choreo_ast (open_out (!outfile_name ^ ".json")) program;
    if !dot_flag then Ast_utils.dot_choreo_ast (open_out (!outfile_name ^ ".dot")) program;
    if !pprint_flag || !json_flag
    then (
      let locs = Ast_utils.extract_locs program in
      List.iter
        (fun loc ->
          let ir = Irgen.epp program loc in
          if !json_flag
          then
            Ast_utils.jsonify_net_ast (open_out (!outfile_name ^ "." ^ loc ^ ".json")) ir;
          if !pprint_flag
          then Ast_utils.pprint_net_ast (open_out (!outfile_name ^ "." ^ loc ^ ".pir")) ir)
        locs))
  else (
    if !pprint_flag then Ast_utils.pprint_choreo_ast stdout program;
    if !json_flag then Ast_utils.jsonify_choreo_ast stdout program;
    if !dot_flag then Ast_utils.dot_choreo_ast stdout program;
    if !pprint_flag || !json_flag
    then (
      let locs = Ast_utils.extract_locs program in
      List.iter
        (fun loc ->
          let ir = Irgen.epp program loc in
          if !json_flag then Ast_utils.jsonify_net_ast stdout ir;
          if !pprint_flag then Ast_utils.pprint_net_ast stdout ir)
        locs))
;;

(*************************************************)

(*context: list of pair of variable name and its binding*)
type local_context = (string * Ast.Local.typ) list
type choreo_context = (string * Ast.Choreo.typ) list

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*return option type*)
let context_lookup ctx var_name = List.assoc_opt var_name ctx

let typeof_bop bop e1 e2 =
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div ->
    (match e1, e2 with
     | Ast.Local.TInt, Ast.Local.TInt -> Some Ast.Local.TInt
     | _ -> None)
  | Ast.Local.Eq
  | Ast.Local.Neq
  | Ast.Local.Lt
  | Ast.Local.Gt
  | Ast.Local.Geq
  | Ast.Local.Leq ->
    (match e1, e2 with
     | Ast.Local.TInt, Ast.Local.TInt -> Some Ast.Local.TBool
     | _ -> None)
  | Ast.Local.And | Ast.Local.Or ->
    (match e1, e2 with
     | Ast.Local.TBool, Ast.Local.TBool -> Some Ast.Local.TBool
     | _ -> None)
;;

let typeof_unop unop e =
  match unop with
  | Ast.Local.Neg ->
    (match e with
     | Ast.Local.TInt -> Some Ast.Local.TInt
     | _ -> None)
  | Ast.Local.Not ->
    (match e with
     | Ast.Local.TBool -> Some Ast.Local.TBool
     | _ -> None)
;;

let rec check_local_pattn ctx p =
  match p with
  | Ast.Local.Default -> Some Ast.Local.TUnit
  | Ast.Local.Val v ->
    (match v with
     | `Int _ -> Some Ast.Local.TInt
     | `String _ -> Some Ast.Local.TString
     | `Bool _ -> Some Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.Pair (p1, p2) ->
    (match check_local_pattn ctx p1, check_local_pattn ctx p2 with
     | Some t1, Some t2 -> Some (Ast.Local.TProd (t1, t2))
     | _ -> None)
  | Ast.Local.Left p | Ast.Local.Right p -> check_local_pattn ctx p

(*type checking of local expression*)
and check_local_exp ctx e =
  match e with
  | Ast.Local.Unit -> Some Ast.Local.TUnit
  | Ast.Local.Val v ->
    (match v with
     | `Int _ -> Some Ast.Local.TInt
     | `String _ -> Some Ast.Local.TString
     | `Bool _ -> Some Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.UnOp (unop, e) ->
    (match check_local_exp ctx e with
     | Some t -> typeof_unop unop t
     | _ -> None)
  | Ast.Local.BinOp (e1, bop, e2) ->
    (match check_local_exp ctx e1, check_local_exp ctx e2 with
     | Some e1_typ, Some e2_typ -> typeof_bop bop e1_typ e2_typ
     | _ -> None)
  | Ast.Local.Let (Ast.Local.VarId var_name, e1, e2) ->
    (match check_local_exp ctx e1 with
     | Some e1_typ -> check_local_exp (add_binding ctx var_name e1_typ) e2
     | _ -> None)
  | Ast.Local.Pair (e1, e2) ->
    (match check_local_exp ctx e1, check_local_exp ctx e2 with
     | Some e1_typ, Some e2_typ -> Some (Ast.Local.TProd (e1_typ, e2_typ))
     | _ -> None)
  | Ast.Local.Fst e ->
    (match check_local_exp ctx e with
     | Some (Ast.Local.TProd (t1, _)) -> Some t1
     | _ -> None)
  | Ast.Local.Snd e ->
    (match check_local_exp ctx e with
     | Some (Ast.Local.TProd (_, t2)) -> Some t2
     | _ -> None)
  | Ast.Local.Left e ->
    (match check_local_exp ctx e with
     | Some t -> Some (Ast.Local.TSum (t, Ast.Local.TUnit))
     | _ -> None)
  | Ast.Local.Right e ->
    (match check_local_exp ctx e with
     | Some t -> Some (Ast.Local.TSum (Ast.Local.TUnit, t))
     | _ -> None)
  | Ast.Local.Match (e, cases) ->
    let match_exp_typ = check_local_exp ctx e in
    let pattn_typs, exp_typs =
      List.split
        (List.map (fun (p, e) -> check_local_pattn ctx p, check_local_exp ctx e) cases)
    in
    if List.for_all (fun x -> x = match_exp_typ) pattn_typs
       && List.for_all (fun x -> x = List.hd exp_typs) exp_typs
    then List.hd exp_typs
    else None
;;
