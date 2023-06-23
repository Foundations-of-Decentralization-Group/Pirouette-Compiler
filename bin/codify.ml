open Ctrl
open Constants
open Utils

exception InvalidProgramException of string

(*
AllowL and AllowR -> if we change it to if then else then, if true then something else (), 
but then and else branch need to have same data type, what to do?
As the final output, are we just returning a unit or printing something? The ocaml code wouldn't run 
For example -> Peron1[L] ~> Person2; Person2.(d+3)
What is the end statment, here we expect Person2 to execute d+3 in local scope, 
But what do we need to do in ocaml code?
*)

let rec _codify (ast: ctrl) (confMap: string list) (currentEntity: string): string = 
  match ast with
  | Value x -> string_of_int x
  | Variable x -> x
  | ChoreoVars x -> x
  | Ret {arg} -> _codify arg confMap currentEntity
  | Unit -> _unit
  | Snd {arg; loc; thn} -> 
    let codified_thn = _codify thn confMap currentEntity in 
    let codified_arg = _codify arg confMap currentEntity in
    _let ^ _space ^ "_" ^ _equals ^ _space ^ _sync ^ _space ^  _lParen ^ _sndmsg ^ _space ^ 
    _space ^ "_channel_" ^ currentEntity ^ _underscore ^ loc ^ _space ^ 
    codified_arg ^ _rParen ^ _space ^ 
    _in ^ _endl ^ codified_thn
  | Rcv {arg; loc; thn} ->
    let codified_thn = _codify thn confMap currentEntity in 
    let codified_arg = _codify arg confMap currentEntity in
      _let ^ _space ^ codified_arg ^ _equals ^ _space ^ _sync ^ _space ^  
      _lParen ^ _rcvmsg ^ _space ^ "_channel_" ^ loc ^ _underscore ^ currentEntity ^ 
       _space ^ _rParen ^ _space ^ _in ^ _space ^
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen 
  | Branch {ift; thn; el} -> 
    let codified_ift = _codify ift confMap currentEntity in 
    let codified_thn = _codify thn confMap currentEntity in
    let codified_el = _codify el confMap currentEntity in
    _if ^ _space ^  codified_ift ^ _endl
    ^_then ^ _space ^  _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ 
    _space ^  _lParen ^ codified_el ^ _rParen
  | Choose {d; loc; thn} -> 
    let codified_thn = _codify thn confMap currentEntity in
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ _lParen ^ 
     _sync ^ _space ^  _lParen ^ _sndmsg ^ _space ^ 
    _space ^ "_channel_" ^ currentEntity ^ _underscore ^ loc ^ _space
    ^ (if d = "L" then "true" else "false") ^ _rParen ^ _rParen ^ _space ^ _in ^ _endl ^ 
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ codified_thn ^ 
    _in ^ _space ^ _unit 
  | AllowL {loc = _; thn = _} ->
    raise (InvalidProgramException "AllowL not permitted.")
  | AllowR {loc = _; thn = _} ->
    raise (InvalidProgramException "AllowR not permitted.")
  | AllowLR {loc; thnL; thnR} ->
    let codified_thnL = _codify thnL confMap currentEntity in
    let codified_thnR = _codify thnR confMap currentEntity in
    _let ^ _space ^ "___synclbl " ^ _equals ^ _space ^ _sync ^ _space ^  
      _lParen ^ _rcvmsg ^ _space ^ "_channel_" ^ loc ^ _underscore ^ currentEntity ^ 
       _space ^ _rParen ^ _space ^ _in ^ _space ^
    _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
    _lParen ^ codified_thnL ^ _rParen ^ _endl ^ _else ^ _space ^ codified_thnR
  | Let {binder = Unit; arg; thn} -> 
    let codified_arg = _codify arg confMap currentEntity in
    let codified_thn = _codify thn confMap currentEntity in 
      _let ^ _space ^ _underscore ^ _equals ^ codified_arg ^ _space ^ _in ^ 
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Let {binder; arg; thn} ->
  let codified_binder = _codify binder confMap currentEntity in 
  let codified_arg = _codify arg confMap currentEntity in
  let codified_thn = _codify thn confMap currentEntity in 
    _let ^ _space ^ codified_binder ^ _equals ^ codified_arg ^ _space ^ _in ^ 
    _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Fun {name; arg; body} ->
    let codified_arg = _codify arg confMap currentEntity in
    let codified_body = _codify body confMap currentEntity in 
      _let ^ _space ^ _rec ^ _space ^ name ^ _space ^ codified_arg ^ _space ^ _equals ^ _space ^ 
      _lParen ^ codified_body ^ _rParen
  | Calling {name; arg} -> 
    let codified_arg = _codify arg confMap currentEntity in
      name ^ _space ^ codified_arg 
  | Application {funct = Fun {name; arg; body}; argument} ->
    let codify_funct = _codify (Fun {name; arg; body}) confMap currentEntity in
    let codified_argument = _codify argument confMap currentEntity in 
      codify_funct ^ _endl ^ _in ^ _space ^ 
      _let ^ _space ^ _disreg ^ _equals ^ name ^ _space ^ codified_argument ^ _space ^ _in ^ _space
      ^ _unit
  | Application {funct = _; argument = _} -> ""
  | Condition {lft; op; rght} ->
    let codified_lft = _codify lft confMap currentEntity in
    let codified_rght = _codify rght confMap currentEntity in 
      _lParen ^ codified_lft ^ _space ^ op ^ _space ^ codified_rght ^ _rParen
  | Plus {lft; rght} ->
    let codified_lft = _codify lft confMap currentEntity in
    let codified_rght = _codify rght confMap currentEntity in 
      _lParen ^ codified_lft ^ _space ^ _plus ^ _space ^ codified_rght ^ _rParen
  | Minus {lft; rght} ->
    let codified_lft = _codify lft confMap currentEntity in
    let codified_rght = _codify rght confMap currentEntity in 
      _lParen ^ codified_lft ^ _space ^ _minus ^ _space ^ codified_rght ^ _rParen 
  | Product {lft; rght} ->
    let codified_lft = _codify lft confMap currentEntity in
    let codified_rght = _codify rght confMap currentEntity in 
      _lParen ^ codified_lft ^ _space ^ _product ^ _space ^ codified_rght ^ _rParen
  | Division {lft; rght} ->
    let codified_lft = _codify lft confMap currentEntity in
    let codified_rght = _codify rght confMap currentEntity in 
      _lParen ^ codified_lft ^ _space ^ _division ^ _space ^ codified_rght ^ _rParen 
  | _ -> ""



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
  code = (Ctrl.Application {
    funct =
    Ctrl.Fun {name = "loop"; arg = (Ctrl.ChoreoVars "X");
      body = Ctrl.Calling {name = "loop"; arg = (Ctrl.ChoreoVars "X")}};
    argument = Ctrl.Ret {arg = (Ctrl.Value 0)}});
  prop = "person1"
}

let _ast2 = Ast{
  code = (
    Ctrl.Let {binder = Ctrl.Unit; arg = Ctrl.Unit;
  thn =
  Ctrl.Rcv {arg = (Ctrl.Variable "d"); loc = "person1";
    thn =
    Ctrl.Branch {
      ift =
      Ctrl.Condition {lft = (Ctrl.Variable "d"); op = "<";
        rght = (Ctrl.Value 10)};
      thn = Ctrl.Choose {d = "L"; loc = "person1"; thn = Ctrl.Unit};
      el = Ctrl.Choose {d = "R"; loc = "person1"; thn = Ctrl.Unit}}}}
  ); prop = "person2"}

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

let () = codify [ast1] file_name 