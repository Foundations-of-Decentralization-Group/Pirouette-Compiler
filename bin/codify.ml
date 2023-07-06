open Ctrl
open Constants
open Utils
open Basictypes

exception InvalidProgramException of string

(*
AllowL and AllowR -> if we change it to if then else then, if true then something else (), 
but then and else branch need to have same data type, what to do?
As the final output, are we just returning a unit or printing something? The ocaml code wouldn't run 
For example -> Peron1[L] ~> Person2; Person2.(d+3)
What is the end statment, here we expect Person2 to execute d+3 in local scope, 
But what do we need to do in ocaml code?
*)

let rec getStrCtrlTyp (typ : ctrlType) (arg : string) : string = 
  (match typ, arg with 
    | Int, _-> "int"
    | Bool, _ -> "bool"
    | String, _ -> "string"
    | CtrlFun (t1, _), "fst" -> getStrCtrlTyp t1 ""
    | CtrlFun (_, t2), "snd" -> getStrCtrlTyp t2 ""
    | CtrlFun (_, _), _ -> ""
  )

let getStrBinop (op : binop) : string = 
  (match op with 
    | Gt -> _gt
    | Lt -> _lt
    | Eq -> _equals
  )


let rec _codify_local (ast: l_ctrl) : string = 
  (match ast with 
    | INT x -> string_of_int x
    | STRING x -> x
    | BOOL x -> string_of_bool x
    (* | Variable (Name x, CtrlFun typ) -> x ^ _colon ^ (getStrCtrlTyp (CtrlFun typ) "fst") *)
    | Variable (Name x, _) -> x 
    | Condition (lft, op, rght, _) ->
      let codified_lft = _codify_local lft in
      let codified_rght = _codify_local rght in 
        _lParen ^ codified_lft ^ _space ^ (getStrBinop op) ^ _space ^ codified_rght ^ _rParen
    | Plus (lft, rght, _) ->
      let codified_lft = _codify_local lft in
      let codified_rght = _codify_local rght in 
        _lParen ^ codified_lft ^ _space ^ _plus ^ _space ^ codified_rght ^ _rParen
    | Minus (lft, rght, _) ->
      let codified_lft = _codify_local lft in
      let codified_rght = _codify_local rght in 
        _lParen ^ codified_lft ^ _space ^ _minus ^ _space ^ codified_rght ^ _rParen 
    | Product (lft, rght, _)->
      let codified_lft = _codify_local lft in
      let codified_rght = _codify_local rght  in 
        _lParen ^ codified_lft ^ _space ^ _product ^ _space ^ codified_rght ^ _rParen
    | Division (lft, rght, _) ->
      let codified_lft = _codify_local lft in
      let codified_rght = _codify_local rght in 
        _lParen ^ codified_lft ^ _space ^ _division ^ _space ^ codified_rght ^ _rParen 
  )

let rec _codify (ast: ctrl) (confMap: string list) (currentEntity: string): string = 
  match ast with
  | ChoreoVars (Name x, typ) -> x ^ _colon ^ (getStrCtrlTyp typ "")
  | Ret (arg, _) -> _codify_local arg
  | Unit -> _unit
  | Snd (arg, Location loc, thn, _) -> 
    let codified_thn = _codify thn confMap currentEntity in 
    let codified_arg = _codify_local arg in
    _let ^ _space ^ "_" ^ _equals ^ _space ^ _sync ^ _space ^  _lParen ^ _sndmsg ^ _space ^ 
    _space ^ "_channel_" ^ currentEntity ^ _underscore ^ loc ^ _space ^ 
    codified_arg ^ _rParen ^ _space ^ 
    _in ^ _endl ^ codified_thn
  | Rcv (arg, Location loc, thn, _) ->
    let codified_thn = _codify thn confMap currentEntity in 
    let codified_arg = _codify_local arg in
      _let ^ _space ^ codified_arg ^ _equals ^ _space ^ _sync ^ _space ^  
      _lParen ^ _rcvmsg ^ _space ^ "_channel_" ^ loc ^ _underscore ^ currentEntity ^ 
       _space ^ _rParen ^ _space ^ _in ^ _space ^
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen 
  | Branch (ift, thn, el, _) -> 
    let codified_ift = _codify ift confMap currentEntity in 
    let codified_thn = _codify thn confMap currentEntity in
    let codified_el = _codify el confMap currentEntity in
    _if ^ _space ^  codified_ift ^ _endl
    ^_then ^ _space ^  _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ 
    _space ^  _lParen ^ codified_el ^ _rParen
  | Choose (Direction d, Location loc, thn, _) -> 
    let codified_thn = _codify thn confMap currentEntity in
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ _lParen ^ 
     _sync ^ _space ^  _lParen ^ _sndmsg ^ _space ^ 
    _space ^ "_channel_" ^ currentEntity ^ _underscore ^ loc ^ _space
    ^ (if d = "L" then "true" else "false") ^ _rParen ^ _rParen ^ _space ^ _in ^ _endl ^ 
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ codified_thn ^ 
    _in ^ _space ^ _unit 
  | AllowL (_, _, _) ->
    raise (InvalidProgramException "AllowL not permitted.")
  | AllowR (_, _, _) ->
    raise (InvalidProgramException "AllowR not permitted.")
  | AllowLR (Location loc, thnL, thnR, _) ->
    let codified_thnL = _codify thnL confMap currentEntity in
    let codified_thnR = _codify thnR confMap currentEntity in
    _let ^ _space ^ "___synclbl"  ^ _equals ^ _space ^ _sync ^ _space ^  
      _lParen ^ _rcvmsg ^ _space ^ "_channel_" ^ loc ^ _underscore ^ currentEntity ^ 
       _space ^ _rParen ^ _space ^ _in ^ _space ^
    _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
    _lParen ^ codified_thnL ^ _rParen ^ _endl ^ _else ^ _space ^ codified_thnR
  | Let (Unit, arg, thn, _) -> 
    let codified_arg = _codify arg confMap currentEntity in
    let codified_thn = _codify thn confMap currentEntity in 
      _let ^ _space ^ _underscore ^ _equals ^ codified_arg ^ _space ^ _in ^ 
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Let (binder, arg, thn, typ) ->
  let codified_binder = _codify binder confMap currentEntity in 
  let codified_arg = _codify arg confMap currentEntity in
  let codified_thn = _codify thn confMap currentEntity in 
    _let ^ _space ^ codified_binder ^ _colon ^ (getStrCtrlTyp typ "") ^ _equals ^ codified_arg ^ _space ^ _in ^ 
    _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Fun (Name name, arg, body, typ) ->
    let codified_arg = _codify arg confMap currentEntity in
    let codified_body = _codify body confMap currentEntity in 
      _let ^ _space ^ _rec ^ _space ^ name ^ _space ^ _lParen ^ codified_arg ^_rParen ^ _space 
      ^ (getStrCtrlTyp typ "snd") ^ _equals ^ _space ^ 
      _lParen ^ codified_body ^ _rParen
  | Calling (Name name, arg, _) -> 
    let codified_arg = _codify arg confMap currentEntity in
      name ^ _space ^ codified_arg 
  | Application (Fun (Name name, arg, body, fun_typ), argument, _) ->
    let codify_funct = _codify (Fun (Name name, arg, body, fun_typ)) confMap currentEntity in
    let codified_argument = _codify argument confMap currentEntity in 
      codify_funct ^ _endl ^ _in ^ _space ^ 
      _let ^ _space ^ _disreg ^ _equals ^ name ^ _space ^ codified_argument ^ _space ^ _in ^ _space
      ^ _unit
  | Application (_, _, _) -> ""



open Printf

let _format_and_save_code code output_file =
  let tmp_input_file = "_tmp.ml" in
  let tmp_output_file = "_tmp_formatted.ml" in

  let ic = open_out tmp_input_file in
  fprintf ic "%s" code;
  close_out ic;

  let command = Printf.sprintf "ocamlformat --enable-outside-detected-project --impl %s -o %s" tmp_input_file tmp_output_file in
  let exit_code = Sys.command command in
  if exit_code <> 0 then begin
    printf "Error executing ocamlformat: %d\n" exit_code;
    Sys.remove tmp_input_file
  end else begin
    Sys.rename tmp_output_file output_file;
    Sys.remove tmp_input_file;
    printf "Code formatted and saved successfully.\n"
  end

let file_name = "config.conf"

type astType = Ast of { code : ctrl; prop : string }

let ast1 = Ast{
  code = (Ctrl.Let (Ctrl.Unit, Ctrl.Unit,
  (Ctrl.AllowLR ((Basictypes.Location "p1"),
     (Ctrl.Rcv ((Ctrl.Variable ((Basictypes.Name "y"), Basictypes.Int)),
        (Basictypes.Location "p1"),
        (Ctrl.Ret ((Ctrl.Variable ((Basictypes.Name "y"), Basictypes.Int)),
           Basictypes.Int)),
        Basictypes.Int)),
     (Ctrl.Rcv ((Ctrl.Variable ((Basictypes.Name "y"), Basictypes.Int)),
        (Basictypes.Location "p1"),
        (Ctrl.Ret ((Ctrl.Variable ((Basictypes.Name "y"), Basictypes.Int)),
           Basictypes.Int)),
        Basictypes.Int)),
     Basictypes.Int)),
  Basictypes.Int));
  prop = "p2"
}

let _ast2 = Ast{
  code = (Ctrl.Let (
    (Ctrl.Ret ((Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
       Basictypes.Int)),
    (Ctrl.Ret ((Ctrl.INT 5), Basictypes.Int)),
    (Ctrl.Branch (
       (Ctrl.Ret (
          (Ctrl.Condition (
             (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
             Basictypes.Gt, (Ctrl.INT 2), Basictypes.Bool)),
          Basictypes.Bool)),
       (Ctrl.Branch (
          (Ctrl.Ret (
             (Ctrl.Condition (
                (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                Basictypes.Gt, (Ctrl.INT 10), Basictypes.Bool)),
             Basictypes.Bool)),
          (Ctrl.Choose ((Basictypes.Direction "L"),
             (Basictypes.Location "p2"),
             (Ctrl.Snd (
                (Ctrl.Plus (
                   (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                   (Ctrl.INT 2), Basictypes.Int)),
                (Basictypes.Location "p2"), Ctrl.Unit, Basictypes.Int)),
             Basictypes.Int)),
          (Ctrl.Choose ((Basictypes.Direction "R"),
             (Basictypes.Location "p2"),
             (Ctrl.Snd (
                (Ctrl.Minus (
                   (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                   (Ctrl.INT 2), Basictypes.Int)),
                (Basictypes.Location "p2"), Ctrl.Unit, Basictypes.Int)),
             Basictypes.Int)),
          Basictypes.Int)),
       (Ctrl.Branch (
          (Ctrl.Ret (
             (Ctrl.Condition (
                (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                Basictypes.Gt, (Ctrl.INT 10), Basictypes.Bool)),
             Basictypes.Bool)),
          (Ctrl.Choose ((Basictypes.Direction "L"),
             (Basictypes.Location "p2"),
             (Ctrl.Snd (
                (Ctrl.Plus (
                   (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                   (Ctrl.INT 2), Basictypes.Int)),
                (Basictypes.Location "p2"), Ctrl.Unit, Basictypes.Int)),
             Basictypes.Int)),
          (Ctrl.Choose ((Basictypes.Direction "R"),
             (Basictypes.Location "p2"),
             (Ctrl.Snd (
                (Ctrl.Minus (
                   (Ctrl.Variable ((Basictypes.Name "x"), Basictypes.Int)),
                   (Ctrl.INT 2), Basictypes.Int)),
                (Basictypes.Location "p2"), Ctrl.Unit, Basictypes.Int)),
             Basictypes.Int)),
          Basictypes.Int)),
       Basictypes.Int)),
    Basictypes.Int)); prop = "p1"}

(* Function to output code for an entity *)
let output_code_for_entity (entity : string) =
  let thread_code = "let " ^ _underscore ^ entity ^ " = Thread.create " ^ entity ^ " () in " in
  thread_code

(* Function to iterate over a list of entities *)
let _getThreadBp (entities : string list) =
  let code_strings = List.map output_code_for_entity entities in
  let joined_code = String.concat "\n" code_strings in
  let join_code = List.map (fun entity -> "Thread.join " ^ _underscore ^ entity ^ ";") entities in
  let joined_join_code = String.concat "\n" join_code in
  joined_code ^ "\n" ^ joined_join_code

let output_code_for_pair (p1 : string) (p2 : string) =
  let channel_p1_p2 = "let _channel_" ^ p1 ^ "_" ^ p2 ^ " = Evt.new_channel () in " in
  let channel_p2_p1 = "let _channel_" ^ p2 ^ "_" ^ p1 ^ " = Evt.new_channel () in " in
  channel_p1_p2 ^ "\n" ^ channel_p2_p1

let _getChannels (entities : string list) =
  let buffer = Buffer.create 256 in
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
  iterate_pairs entities;
  Buffer.contents buffer

let codify asts file_name =
  let confMap = read_config_file file_name in
  let code = _threadImport ^ _commonBp ^ _lParen ^
              _getChannels confMap ^ 
              (List.map (fun ast -> match ast with | Ast{code; prop} -> 
                _let ^ _space ^ prop ^ "() = " ^ 
                _lParen ^ _codify code confMap prop ^ _rParen ^ _space ^ _in ^ _space) asts 
                |> String.concat "\n \n") ^ _getThreadBp confMap ^ _rParen in
  let _output_file = "bin/output.ml" in
  (* print_endline code  *)
  _format_and_save_code code _output_file

let () = codify [ast1; _ast2] file_name 