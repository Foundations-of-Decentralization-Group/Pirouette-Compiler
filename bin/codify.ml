open Ctrl
open Constants

let rec _codify (ast: ctrl) : string = 
  match ast with
  | Value x -> string_of_int x
  | Variable x -> x
  | ChoreoVars x -> x
  | Ret x -> _codify x
  | Unit -> _unit
  | Snd {arg; loc; thn} -> 
    let codified_thn = _codify thn in 
    let codified_arg = _codify arg in
    _lParen ^ _sndmsg ^ _space ^ codified_arg ^ _space ^ loc ^ _rParen ^ _endl ^ codified_thn
  | Rcv {arg; loc; thn} ->
    let codified_thn = _codify thn in 
    let codified_arg = _codify arg in
      _let ^ _space ^ codified_arg ^ _equals ^ _rcvmsg ^ _space ^ loc ^ _space ^ _in ^ _space ^
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Branch {ift; thn; el} -> 
    let codified_ift = _codify ift in 
    let codified_thn = _codify thn in
    let codified_el = _codify el in
    _if ^ _space ^  codified_ift ^ _endl
    ^_then ^ _space ^  _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ 
    _space ^  _lParen ^ codified_el ^ _rParen
  | Choose {d; loc; thn} -> 
    let codified_thn = _codify thn in
    _lParen ^ _sndmsg ^ _space ^ d ^ _space ^ loc ^ _rParen ^ _endl ^ codified_thn
  | Let {binder; arg; thn} ->
    let codified_binder = _codify binder in 
    let codified_arg = _codify arg in
    let codified_thn = _codify thn in 
      _let ^ _space ^ codified_binder ^ _equals ^ codified_arg ^ _space ^ _in ^ 
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
  | Fun {name; arg; body} ->
    let codified_arg = _codify arg in
    let codified_body = _codify body in 
      _let ^ _space ^ name ^ _space ^ codified_arg ^ _space ^ _equals ^ _space ^ 
      _lParen ^ codified_body ^ _rParen
  | Application {funct = Fun {name; arg; body}; argument} ->
    let codified_arg = _codify arg in
    let codified_body = _codify body in 
    let codified_argument = _codify argument in 
    _let ^ name ^ _space ^ codified_arg ^ _space ^ _equals ^ _space ^ 
    _lParen ^ codified_body ^ _rParen ^ _endl ^_endl ^ name ^ _space ^ codified_argument
  | Application {funct = _; argument = _} -> ""
  | Condition {lft; op; rght} ->
    let codified_lft = _codify lft in
    let codified_rght = _codify rght in 
      _lParen ^ codified_lft ^ _space ^ op ^ _space ^ codified_rght ^ _rParen
  | Plus {lft; rght} ->
    let codified_lft = _codify lft in
    let codified_rght = _codify rght in 
      _lParen ^ codified_lft ^ _space ^ _plus ^ _space ^ codified_rght ^ _rParen
  | Minus {lft; rght} ->
    let codified_lft = _codify lft in
    let codified_rght = _codify rght in 
      _lParen ^ codified_lft ^ _space ^ _minus ^ _space ^ codified_rght ^ _rParen 
  | Product {lft; rght} ->
    let codified_lft = _codify lft in
    let codified_rght = _codify rght in 
      _lParen ^ codified_lft ^ _space ^ _product ^ _space ^ codified_rght ^ _rParen
  | Division {lft; rght} ->
    let codified_lft = _codify lft in
    let codified_rght = _codify rght in 
      _lParen ^ codified_lft ^ _space ^ _division ^ _space ^ codified_rght ^ _rParen 
  | _ -> ""


let ast = (Ctrl.Fun {name = "funname"; arg = (Ctrl.ChoreoVars "X_0");
body =
Ctrl.Rcv {arg = (Ctrl.Variable "l"); loc = "Seller";
  thn = Ctrl.Choose {d = "R"; loc = "Seller"; thn = (Ctrl.ChoreoVars "X")}}})

let () = print_endline (_codify ast)