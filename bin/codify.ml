open Ctrl
open Constants
open Hashtbl
open Utils


(*
AllowL and AllowR -> if we change it to if then else then, if true then something else (), 
but then and else branch need to have same data type, what to do?
As the final output, are we just returning a unit or printing something? The ocaml code wouldn't run 
For example -> Peron1[L] ~> Person2; Person2.(d+3)
What is the end statment, here we expect Person2 to execute d+3 in local scope, 
But what do we need to do in ocaml code?
*)

let rec _codify (ast: ctrl) (confMap: (string, int) Hashtbl.t): string = 
  match ast with
  | Value x -> string_of_int x
  | Variable x -> x
  | ChoreoVars x -> x
  | Ret {arg} -> _codify arg confMap
  | Unit -> _unit
  | Snd {arg; loc; thn} -> 
    let codified_thn = _codify thn confMap in 
    let codified_arg = _codify arg confMap in
    _let ^ _space ^ "_" ^ _equals ^ _lParen ^ _sndmsg ^ _space ^ 
    codified_arg ^ _space ^ string_of_int (find confMap loc) ^ _rParen ^ _space ^ 
    _in ^ _endl ^ codified_thn
  | Rcv {arg; loc; thn} ->
    let codified_thn = _codify thn confMap in 
    let codified_arg = _codify arg confMap in
    _serverBoilerPlate (string_of_int (find confMap loc)) ^ 
      _let ^ _space ^ codified_arg ^ _equals ^ _rcvmsg ^ _space ^ "___server_sock" ^ _space ^ _in ^ _space ^
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen 
  | Branch {ift; thn; el} -> 
    let codified_ift = _codify ift confMap in 
    let codified_thn = _codify thn confMap in
    let codified_el = _codify el confMap in
    _if ^ _space ^  codified_ift ^ _endl
    ^_then ^ _space ^  _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ 
    _space ^  _lParen ^ codified_el ^ _rParen
  | Choose {d; loc; thn} -> 
    let codified_thn = _codify thn confMap in
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ _lParen ^ _sndmsg ^ _space ^ (if d = "L" then "\"true\"" else "\"false\"") ^ 
    _space ^ string_of_int (find confMap loc) ^ _rParen ^ _space ^ _in ^ _endl ^ 
    _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ codified_thn ^ 
    _in ^ _space ^ _unit
  | AllowL {loc; thn} ->
    let codified_thn = _codify thn confMap in
    _serverBoilerPlate (string_of_int (find confMap loc)) ^ 
      _let ^ _space ^ "___synclbl" ^ _equals ^ _rcvmsg ^ _space ^ 
      "___server_sock" ^ _space ^ _in ^ _space ^
    _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
    _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ _space ^ _unit
  | AllowR {loc; thn} ->
    let codified_thn = _codify thn confMap in
    _serverBoilerPlate (string_of_int (find confMap loc)) ^ 
      _let ^ _space ^ "___synclbl" ^ _equals ^ _rcvmsg ^ _space ^ 
      "___server_sock" ^ _space ^ _in ^ _space ^
    _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
    _lParen ^ _unit ^ _rParen ^ _endl ^ _else ^ _space ^ codified_thn
  | AllowLR {loc; thnL; thnR} ->
    let codified_thnL = _codify thnL confMap in
    let codified_thnR = _codify thnR confMap in
    _serverBoilerPlate (string_of_int (find confMap loc)) ^ 
      _let ^ _space ^ "___synclbl" ^ _equals ^ _rcvmsg ^ _space ^ 
      "___server_sock" ^ _space ^ _in ^ _space ^
    _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
    _lParen ^ codified_thnL ^ _rParen ^ _endl ^ _else ^ _space ^ codified_thnR
  | Let {binder = Unit; arg; thn} -> 
    let codified_arg = _codify arg confMap in
    let codified_thn = _codify thn confMap in 
      _let ^ _space ^ _underscore ^ _equals ^ codified_arg ^ _space ^ _in ^ 
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Let {binder; arg; thn} ->
  let codified_binder = _codify binder confMap in 
  let codified_arg = _codify arg confMap in
  let codified_thn = _codify thn confMap in 
    _let ^ _space ^ codified_binder ^ _equals ^ codified_arg ^ _space ^ _in ^ 
    _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Fun {name; arg; body} ->
    let codified_arg = _codify arg confMap in
    let codified_body = _codify body confMap in 
      _let ^ _space ^ name ^ _space ^ codified_arg ^ _space ^ _equals ^ _space ^ 
      _lParen ^ codified_body ^ _rParen
  | Application {funct = Fun {name; arg; body}; argument} ->
    let codified_arg = _codify arg confMap in
    let codified_body = _codify body confMap in 
    let codified_argument = _codify argument confMap in 
    _let ^ _space ^ name ^ _space ^ codified_arg ^ _equals ^ _space ^
    _lParen ^ codified_body ^ _rParen ^ _endl ^ _in ^ _space ^ 
    _let ^ _space ^ _disreg ^ _equals ^ name ^ _space ^ codified_argument ^ _space ^ _in ^ _space
    ^ _unit
  | Application {funct = _; argument = _} -> ""
  | Condition {lft; op; rght} ->
    let codified_lft = _codify lft confMap in
    let codified_rght = _codify rght confMap in 
      _lParen ^ codified_lft ^ _space ^ op ^ _space ^ codified_rght ^ _rParen
  | Plus {lft; rght} ->
    let codified_lft = _codify lft confMap in
    let codified_rght = _codify rght confMap in 
      _lParen ^ codified_lft ^ _space ^ _plus ^ _space ^ codified_rght ^ _rParen
  | Minus {lft; rght} ->
    let codified_lft = _codify lft confMap in
    let codified_rght = _codify rght confMap in 
      _lParen ^ codified_lft ^ _space ^ _minus ^ _space ^ codified_rght ^ _rParen 
  | Product {lft; rght} ->
    let codified_lft = _codify lft confMap in
    let codified_rght = _codify rght confMap in 
      _lParen ^ codified_lft ^ _space ^ _product ^ _space ^ codified_rght ^ _rParen
  | Division {lft; rght} ->
    let codified_lft = _codify lft confMap in
    let codified_rght = _codify rght confMap in 
      _lParen ^ codified_lft ^ _space ^ _division ^ _space ^ codified_rght ^ _rParen 
  | _ -> ""


let ast = (Ctrl.Let {binder = (Ctrl.Variable "amt_due");
arg = Ctrl.Ret {arg = (Ctrl.Variable "\"5\"")};
thn =
Ctrl.Snd {arg = (Ctrl.Variable "amt_due"); loc = "Person2";
  thn =
  Ctrl.AllowL {loc = "Person2";
    thn =
    Ctrl.Ret {
      arg =
      Ctrl.Plus {lft = (Ctrl.Variable "amt_due"); rght = (Ctrl.Value 3)}}}}})

(* let () = print_endline (_codify ast) *)

open Printf
(* open Unix *)

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

(* let () =
let code = _boilerPlate ^ _lParen ^ _serverBoilerPlate (find confMap "Buyer") ^ (_codify ast) ^ _rParen in
let output_file = "output.ml" in
format_and_save_code code output_file *)

let file_name = "config.conf"

let () = 
  let confMap = read_config_file file_name in
  let code = _boilerPlate ^ _lParen ^ (_codify ast confMap) ^ _rParen in
  let output_file = "output.ml" in
  _format_and_save_code code output_file