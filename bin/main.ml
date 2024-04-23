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

(*err msg of type checking*)
type typ_err =
  | UnboundVariable
  | BopErr
  | LocalPattnMismatch
  | LocalExpErr

(*context: list of pair of variable name and its binding*)
type local_context = (string * Ast.Local.typ) list
type choreo_context = (string * Ast.Choreo.typ) list

(*construct and raise exception with error message*)
let handle_err typ_err arg =
  let err_msg =
    match typ_err with
    | UnboundVariable -> "Unbound variable: " ^ arg
    | BopErr -> "operator and operands mismatch, expected: " ^ arg
    | LocalPattnMismatch -> "Pattern and expression mismatchL: " ^ arg
    | LocalExpErr -> "Error in local expression, detail: " ^ arg
  in
  failwith err_msg

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*raise exception if pair is not found*)
let context_lookup ctx var_name =
  match List.assoc_opt var_name ctx with
  | Some t -> t
  | None -> handle_err UnboundVariable var_name

let typeof_bop bop e1 e2 =
  match bop with
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ast.Local.TInt
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TInt")
  | Ast.Local.Eq | Ast.Local.Neq | Ast.Local.Lt | Ast.Local.Gt | Ast.Local.Geq
  | Ast.Local.Leq -> (
      match (e1, e2) with
      | Ast.Local.TInt, Ast.Local.TInt -> Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TInt -> Local.TInt -> Local.TBool")
  | Ast.Local.And | Ast.Local.Or -> (
      match (e1, e2) with
      | Ast.Local.TBool, Ast.Local.TBool -> Ast.Local.TBool
      | _ -> handle_err BopErr "Local.TBool -> Local.TBool -> Local.TBool")

let rec check_local_pattn ctx p =
  match p with
  | Ast.Local.Default -> Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ast.Local.TInt
      | `String _ -> Ast.Local.TString
      | `Bool _ -> Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.Pair (p1, p2) ->
      Ast.Local.TProd (check_local_pattn ctx p1, check_local_pattn ctx p2)
  (*TODO: not sure left and right*)
  | Ast.Local.Left p -> (
      match check_local_pattn ctx p with _ -> Ast.Local.TUnit)
  | Ast.Local.Right p -> (
      match check_local_pattn ctx p with _ -> Ast.Local.TUnit)

and check_local_case ctx match_typ p e =
  let p_typ = check_local_pattn ctx p in
  if p_typ = match_typ then check_local_exp ctx e
  else handle_err LocalPattnMismatch "Pattern and expression mismatch"

(*type checking of local expression*)
and check_local_exp ctx e =
  match e with
  | Ast.Local.Unit -> Ast.Local.TUnit
  | Ast.Local.Val v -> (
      match v with
      | `Int _ -> Ast.Local.TInt
      | `String _ -> Ast.Local.TString
      | `Bool _ -> Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.UnOp (unop, e) -> (
      match unop with
      | Ast.Local.Neg -> (
          match check_local_exp ctx e with
          | Ast.Local.TInt -> Ast.Local.TInt
          | _ -> handle_err LocalExpErr "Expect Local.TInt")
      | Ast.Local.Not -> (
          match check_local_exp ctx e with
          | Ast.Local.TBool -> Ast.Local.TBool
          | _ -> handle_err LocalExpErr "Expect Local.TBool"))
  | Ast.Local.BinOp (e1, bop, e2) ->
      let e1_typ, e2_typ = (check_local_exp ctx e1, check_local_exp ctx e2) in
      typeof_bop bop e1_typ e2_typ
  | Ast.Local.Let (Ast.Local.VarId var_name, e1, e2) ->
      let e1_typ = check_local_exp ctx e1 in
      check_local_exp (add_binding ctx var_name e1_typ) e2
  | Ast.Local.Pair (e1, e2) ->
      let e1_typ, e2_typ = (check_local_exp ctx e1, check_local_exp ctx e2) in
      Ast.Local.TProd (e1_typ, e2_typ)
  | Ast.Local.Fst e -> (
      match check_local_exp ctx e with
      | Ast.Local.TProd (t1, _) -> t1
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Snd e -> (
      match check_local_exp ctx e with
      | Ast.Local.TProd (_, t2) -> t2
      | _ -> handle_err LocalExpErr "Expect Local Pair")
  | Ast.Local.Left e -> Ast.Local.TSum (check_local_exp ctx e, Ast.Local.TUnit)
  | Ast.Local.Right e -> Ast.Local.TSum (Ast.Local.TUnit, check_local_exp ctx e)
  | Ast.Local.Match (e, cases) ->
      let e_typ = check_local_exp ctx e in
      let _ = List.map (fun (p, e) -> check_local_case ctx e_typ p e) cases in
      e_typ

