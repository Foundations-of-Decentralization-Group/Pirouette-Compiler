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

(*dummy metainfo to let compiler happy*)
let m : Ast.Metainfo.metainfo = "", 0

(*context: list of pair of variable name and its binding*)
type local_context = (string * Ast.Local.typ) list
type choreo_context = (string * Ast.Choreo.typ) list
type global_context = (string * string * Ast.Local.typ) list

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*return Result type*)
and ctx_lookup ctx var_name =
  try Ok (List.assoc var_name ctx) with
  | Not_found -> Error "Variable not found"

(*extract loc_id's local context from glocal context*)
and extract_local_ctx global_ctx loc_id =
  List.fold_right
    (fun (loc, var_name, typ) acc -> if loc = loc_id then (var_name, typ) :: acc else acc)
    global_ctx
    []
;;

(* ============================== Local ============================== *)
let rec check_local_expr ctx expected_typ = function
  | Ast.Local.Unit _ -> expected_typ = Ast.Local.TUnit m
  | Ast.Local.Val (v, _) -> expected_typ = typeof_Val v
  | Ast.Local.Var (v, _) ->
    (match ctx_lookup ctx v with
     | Ok t -> expected_typ = t
     | _ -> false)
  | Ast.Local.UnOp (op, e, _) ->
    (match op with
     | Ast.Local.Neg _ ->
       check_local_expr ctx (TInt m) e && expected_typ = Ast.Local.TInt m
     | Ast.Local.Not _ ->
       check_local_expr ctx (Ast.Local.TBool m) e && expected_typ = Ast.Local.TBool m)
  | Ast.Local.BinOp (e1, op, e2, _) ->
    (match op with
     | Ast.Local.Plus _ | Ast.Local.Minus _ | Ast.Local.Times _ | Ast.Local.Div _ ->
       check_local_expr ctx (Ast.Local.TInt m) e1
       && check_local_expr ctx (Ast.Local.TInt m) e2
       && expected_typ = Ast.Local.TInt m
     | Ast.Local.Eq _
     | Ast.Local.Neq _
     | Ast.Local.Lt _
     | Ast.Local.Leq _
     | Ast.Local.Gt _
     | Ast.Local.Geq _ ->
       check_local_expr ctx (Ast.Local.TInt m) e1
       && check_local_expr ctx (Ast.Local.TInt m) e2
       && expected_typ = Ast.Local.TBool m
     | Ast.Local.And _ | Ast.Local.Or _ ->
       check_local_expr ctx (Ast.Local.TBool m) e1
       && check_local_expr ctx (Ast.Local.TBool m) e2
       && expected_typ = Ast.Local.TBool m)
  | Ast.Local.Let (Ast.Local.VarId (var_name, _), e1, e2, _) ->
    check_local_expr (add_binding ctx var_name (typeof_Val e1 m)) expected_typ e2
  | _ -> false

and typeof_Val = function
  | Int _ -> Ast.Local.TInt m
  | Bool _ -> Ast.Local.TBool m
  | String _ -> Ast.Local.TString m
;;
