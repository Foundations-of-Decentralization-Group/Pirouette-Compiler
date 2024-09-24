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
type global_context = (Ast.Local.loc_id * string * Ast.Local.typ) list

(*add varname:vartype pair to context list*)
let add_binding ctx var_name var_type = (var_name, var_type) :: ctx

(*lookup varname in context list to get its binding*)
(*return Result type*)
and context_lookup ctx var_name =
  try Ok (List.assoc var_name ctx) with
  | Not_found -> Error "Variable not found"

(* let global_context_lookup global_ctx loc_id =
  let filtered = List.filter (fun (loc, _, _) -> loc = loc_id) global_ctx in
  List.map (fun (_, var_name, typ) -> (var_name, typ)) filtered *)

let global_context_lookup global_ctx loc_id =
  List.fold_right (fun (loc, var_name, typ) acc ->
    if loc = loc_id then (var_name, typ) :: acc else acc
) global_ctx []

(* ============================== Local ============================== *)
let rec check_local_pattn ctx = function
  | Ast.Local.Default -> Ok Ast.Local.TUnit
  | Ast.Local.Val v ->
    (match v with
     | `Int _ -> Ok Ast.Local.TInt
     | `String _ -> Ok Ast.Local.TString
     | `Bool _ -> Ok Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.Pair (p1, p2) ->
    (match check_local_pattn ctx p1, check_local_pattn ctx p2 with
     | Ok t1, Ok t2 -> Ok (Ast.Local.TProd (t1, t2))
     | Error errmsg, Ok _ | Ok _, Error errmsg -> Error errmsg
     | Error errmsg1, Error errmsg2 -> Error (errmsg1 ^ "\n" ^ errmsg2))
  | Ast.Local.Left p | Ast.Local.Right p -> check_local_pattn ctx p

and check_local_exp ctx = function
  | Ast.Local.Unit -> Ok Ast.Local.TUnit
  | Ast.Local.Val v ->
    (match v with
     | `Int _ -> Ok Ast.Local.TInt
     | `String _ -> Ok Ast.Local.TString
     | `Bool _ -> Ok Ast.Local.TBool)
  | Ast.Local.Var (Ast.Local.VarId var_name) -> context_lookup ctx var_name
  | Ast.Local.UnOp (unop, e) ->
    (match check_local_exp ctx e with
     | Ok t -> typeof_unop t unop
     | Error errmsg -> Error errmsg)
  | Ast.Local.BinOp (e1, bop, e2) ->
    (match check_local_exp ctx e1, check_local_exp ctx e2 with
     | Ok e1_typ, Ok e2_typ -> typeof_bop e1_typ e2_typ bop
     | Error errmsg, Ok _ | Ok _, Error errmsg -> Error errmsg
     | Error errmsg1, Error errmsg2 -> Error (errmsg1 ^ "\n" ^ errmsg2))
  | Ast.Local.Let (Ast.Local.VarId var_name, e1, e2) ->
    (match check_local_exp ctx e1 with
     | Ok e1_typ -> check_local_exp (add_binding ctx var_name e1_typ) e2
     | Error errmsg -> Error errmsg)
  | Ast.Local.Pair (e1, e2) ->
    (match check_local_exp ctx e1, check_local_exp ctx e2 with
     | Ok e1_typ, Ok e2_typ -> Ok (Ast.Local.TProd (e1_typ, e2_typ))
     | Error errmsg, Ok _ | Ok _, Error errmsg -> Error errmsg
     | Error errmsg1, Error errmsg2 -> Error (errmsg1 ^ "\n" ^ errmsg2))
  | Ast.Local.Fst e ->
    (match check_local_exp ctx e with
     | Ok (Ast.Local.TProd (t1, _)) -> Ok t1
     | Ok _ -> Error "Expected: TProd (t1, _) -> T1"
     | Error msg -> Error msg)
  | Ast.Local.Snd e ->
    (match check_local_exp ctx e with
     | Ok (Ast.Local.TProd (_, t2)) -> Ok t2
     | Ok _ -> Error "Expected: TProd (_, t2) -> T2"
     | Error msg -> Error msg)
  | Ast.Local.Left e ->
    (match check_local_exp ctx e with
     | Ok (Ast.Local.TSum (t1, _)) -> Ok t1
     | Ok _ -> Error "Expected: TSum (t1, _) -> T1"
     | Error msg -> Error msg)
  | Ast.Local.Right e ->
    (match check_local_exp ctx e with
     | Ok (Ast.Local.TSum (_, t2)) -> Ok t2
     | Ok _ -> Error "Expected: TSum (_, t2) -> T2"
     | Error msg -> Error msg)
  | Ast.Local.Match (e, cases) ->
    let match_exp_typ = check_local_exp ctx e in
    let pattn_typs, exp_typs =
      List.split
        (List.map (fun (p, e) -> check_local_pattn ctx p, check_local_exp ctx e) cases)
    in
    (match List.for_all (fun x -> x = match_exp_typ) pattn_typs with
     | false -> Error "Pattern types do not match"
     | true ->
       (match List.for_all (fun x -> x = List.hd exp_typs) exp_typs with
        | false -> Error "Expression types do not match"
        | true -> List.hd exp_typs))

and typeof_bop e1 e2 = function
  | Ast.Local.Plus | Ast.Local.Minus | Ast.Local.Times | Ast.Local.Div ->
    (match e1, e2 with
     | Ast.Local.TInt, Ast.Local.TInt -> Ok Ast.Local.TInt
     | _ -> Error "Expected: TInt -> TInt -> TInt")
  | Ast.Local.Eq
  | Ast.Local.Neq
  | Ast.Local.Lt
  | Ast.Local.Gt
  | Ast.Local.Geq
  | Ast.Local.Leq ->
    (match e1, e2 with
     | Ast.Local.TInt, Ast.Local.TInt -> Ok Ast.Local.TBool
     | _ -> Error "Expected: TInt -> TInt -> TBool")
  | Ast.Local.And | Ast.Local.Or ->
    (match e1, e2 with
     | Ast.Local.TBool, Ast.Local.TBool -> Ok Ast.Local.TBool
     | _ -> Error "Expected: TBool -> TBool -> TBool")

and typeof_unop e = function
  | Ast.Local.Neg ->
    (match e with
     | Ast.Local.TInt -> Ok Ast.Local.TInt
     | _ -> Error "Expected: TInt -> TInt")
  | Ast.Local.Not ->
    (match e with
     | Ast.Local.TBool -> Ok Ast.Local.TBool
     | _ -> Error "Expected: TBool -> TBool")
;;

(* ============================== Choreo ============================== *)

let rec check_choreo_pattn choreo_ctx local_ctx global_ctx = function
  | Ast.Choreo.Default -> Ok Ast.Choreo.TUnit
  | Ast.Choreo.Var (Ast.Local.VarId var_name) -> context_lookup choreo_ctx var_name
  | Ast.Choreo.Pair (p1, p2) -> 
    (match check_choreo_pattn choreo_ctx local_ctx global_ctx p1, check_choreo_pattn choreo_ctx local_ctx global_ctx p2 with 
    | Ok t1, Ok t2 -> Ok (Ast.Choreo.TProd(t1, t2))
    | Error errmsg, Ok _ | Ok _, Error errmsg -> Error errmsg
    | Error errmsg1, Error errmsg2 -> Error (errmsg1 ^ "\n" ^ errmsg2))
  | Ast.Choreo.LocPatt (Ast.Local.LocId loc_name, p1) -> 
    let choreo_ctx_locid = global_context_lookup global_ctx loc_name in
    (match check_local_pattn local_ctx p1 with 
    | Ok _ -> context_lookup choreo_ctx_locid loc_name 
    | Error errmsg -> Error errmsg)
  | Ast.Choreo.Left p | Ast.Choreo.Right p -> check_choreo_pattn choreo_ctx local_ctx global_ctx p

and check_choreo_exp choreo_ctx local_ctx global_ctx = function
  | Ast.Choreo.Unit -> Ok Ast.Choreo.TUnit
  | Ast.Choreo.Var (Ast.Local.VarId var_name) -> context_lookup choreo_ctx var_name
  | Ast.Choreo.LocExpr (loc_name, e1) -> (
    match check_local_exp local_ctx e1 with
    | Ok local_etyp -> Ok (Ast.Choreo.TLoc (loc_name, local_etyp))
    | Error errmsg -> Error errmsg)
  | Ast.Choreo.Send (loc_name1, e1, loc_name2) -> (
    let choreo_ctx1 = global_context_lookup global_ctx loc_name1 in
    let choreo_ctx2 = global_context_lookup global_ctx loc_name2 in
    (match check_choreo_exp choreo_ctx1 local_ctx global_ctx e1 with
      | Ok typ1 -> 
        (match check_choreo_exp choreo_ctx2 local_ctx global_ctx e1 with
          | Ok typ2 when typ1 = typ2 -> Ok Ast.Choreo.TUnit
          | Ok _ -> Error "Expression type do not match the with the two local IDs"
          | Error errmsg -> Error errmsg)
      | Error errmsg -> Error errmsg))
  | Ast.Choreo.Sync (loc_name1, _, loc_name2, e1) -> (
    let choreo_ctx1 = global_context_lookup global_ctx loc_name1 in
    let choreo_ctx2 = global_context_lookup global_ctx loc_name2 in
    (match check_choreo_exp choreo_ctx1 local_ctx global_ctx e1 with 
      | Ok typ1 -> 
          (match check_choreo_exp choreo_ctx2 local_ctx global_ctx e1 with 
            | Ok typ2 when typ1 = typ2 -> Ok Ast.Choreo.TUnit
            | Ok _ -> Error "Expression type do not match the with the two local IDs"
            | Error errmsg -> Error errmsg)
      | Error errmsg -> Error errmsg))
  | Ast.Choreo.If (e1, e2, e3) -> Ok Ast.Choreo.TUnit
    (*check if e1 is LocExpr of ID id with Local.expr e *)
    (*then gbl_ctx_lookup to get the local_ctx of id*)
    (*then local_exp_check if e is BinOp*)
    (*then typeof_binop e is boolean*)

    (*check if e2 e3 are same Choreo typ, then return type of e2*)


    (* (match check_choreo_exp choreo_ctx local_ctx global_ctx e1 with 
      | Ok Ast.Choreo.TLoc (lname, Ast.Local.TBool) 
        (match check_choreo_exp choreo_ctx local_ctx global_ctx e2, check_choreo_exp choreo_ctx local_ctx global_ctx e3 with 
          | Ok t2, Ok t3 when t2 = t3 -> Ok t2
          | Ok _, Ok _ -> Error "Expression types do not match"
          | _, _ -> Error "Type Error")
      | Ok _ -> Error "The condition of an if expression must be a boolean"
      | Error errmsg -> Error errmsg) *)
  | Ast.Choreo.Let (stmt_block, e1) -> Ok Ast.Choreo.TUnit
  | Ast.Choreo.FunDef (pattn_list, e1) -> Ok Ast.Choreo.TUnit
  | Ast.Choreo.FunApp (e1, e2) -> Ok Ast.Choreo.TUnit
  | Ast.Choreo.Pair (e1, e2) ->
    (match check_choreo_exp choreo_ctx local_ctx global_ctx e1, check_choreo_exp choreo_ctx local_ctx global_ctx e2 with
      | Ok e1_typ, Ok e2_typ -> Ok (Ast.Choreo.TProd (e1_typ, e2_typ))
      | Error errmsg, Ok _ | Ok _, Error errmsg -> Error errmsg
      | Error errmsg1, Error errmsg2 -> Error (errmsg1 ^ "\n" ^ errmsg2))
  | Ast.Choreo.Fst e ->
    (match check_choreo_exp choreo_ctx local_ctx global_ctx e with
      | Ok (Ast.Choreo.TProd (t1, _)) -> Ok t1
      | Ok _ -> Error "Expected: TProd (t1, _) -> T1"
      | Error msg -> Error msg)
  | Ast.Choreo.Snd e ->
    (match check_choreo_exp choreo_ctx local_ctx global_ctx e with
      | Ok (Ast.Choreo.TProd (_, t2)) -> Ok t2
      | Ok _ -> Error "Expected: TProd (_, t2) -> T2"
      | Error msg -> Error msg)
  | Ast.Choreo.Left e ->
    (match check_choreo_exp choreo_ctx local_ctx global_ctx e with
      | Ok (Ast.Choreo.TSum (t1, _)) -> Ok t1
      | Ok _ -> Error "Expected: TSum (t1, _) -> T1"
      | Error msg -> Error msg)
  | Ast.Choreo.Right e ->
    (match check_choreo_exp choreo_ctx local_ctx global_ctx e with
      | Ok (Ast.Choreo.TSum (_, t2)) -> Ok t2
      | Ok _ -> Error "Expected: TSum (_, t2) -> T2"
      | Error msg -> Error msg)
  | Ast.Choreo.Match (e, cases) ->
    let match_exp_typ = check_choreo_exp choreo_ctx local_ctx global_ctx e in
    let pattn_typs, exp_typs =
      List.split
        (List.map (fun (p, e) -> check_choreo_pattn choreo_ctx local_ctx global_ctx p, check_choreo_exp choreo_ctx local_ctx global_ctx e) cases)
    in
    (match List.for_all (fun x -> x = match_exp_typ) pattn_typs with
      | false -> Error "Pattern types do not match"
      | true ->
        (match List.for_all (fun x -> x = List.hd exp_typs) exp_typs with
        | false -> Error "Expression types do not match"
        | true -> List.hd exp_typs))
;;
