open Backend_intf
open Constants
open Controlang.Ctrl
open Controlang.LocalCtrl
open Basictypes
open Printf


exception InvalidProgramException of string

module Ocaml_backend(Com : Comm) : Backend = struct

  let ext = ".ml"

  let rec ctrl_to_local_type (typ : Controlang.ctrlType) (arg : string) : string = 
    (match typ, arg with 
      | Int, _-> "int"
      | Bool, _ -> "bool"
      | String, _ -> "string"
      | CtrlFun (t1, _), "fst" -> ctrl_to_local_type t1 ""
      | CtrlFun (_, t2), "snd" -> ctrl_to_local_type t2 ""
      | CtrlFun (_, _), _ -> ""
    )
  
  let getStrBinop (op : binop) : string = 
    (match op with 
      | Gt -> _gt
      | Lt -> _lt
      | Eq -> _equals
    )
  
  let rec codify_local (ast: l_ctrl) : string = 
    (match ast with 
      | INT x -> string_of_int x
      | STRING x -> x
      | BOOL x -> string_of_bool x
      (* | Variable (Name x, CtrlFun typ) -> x ^ _colon ^ (ctrl_to_local_type (CtrlFun typ) "fst") *)
      | Variable (Name x, _) -> x 
      | Condition (lft, op, rght, _) ->
        let codified_lft = codify_local lft in
        let codified_rght = codify_local rght in 
          _lParen ^ codified_lft ^ _space ^ (getStrBinop op) ^ _space ^ codified_rght ^ _rParen
      | Plus (lft, rght, _) ->
        let codified_lft = codify_local lft in
        let codified_rght = codify_local rght in 
          _lParen ^ codified_lft ^ _space ^ _plus ^ _space ^ codified_rght ^ _rParen
      | Minus (lft, rght, _) ->
        let codified_lft = codify_local lft in
        let codified_rght = codify_local rght in 
          _lParen ^ codified_lft ^ _space ^ _minus ^ _space ^ codified_rght ^ _rParen 
      | Product (lft, rght, _)->
        let codified_lft = codify_local lft in
        let codified_rght = codify_local rght  in 
          _lParen ^ codified_lft ^ _space ^ _product ^ _space ^ codified_rght ^ _rParen
      | Division (lft, rght, _) ->
        let codified_lft = codify_local lft in
        let codified_rght = codify_local rght in 
          _lParen ^ codified_lft ^ _space ^ _division ^ _space ^ codified_rght ^ _rParen 
    )

  let rec codify (ast: ctrl) (confMap: string list) (currentEntity: string): string = 
    match ast with
    | ChoreoVars (Name x, typ) -> x ^ _colon ^ (ctrl_to_local_type typ "")
    | Ret (arg, _) -> codify_local arg
    | Unit -> _unit
    | Snd (arg, Location loc, thn, _) -> 
      let codified_thn = codify thn confMap currentEntity in 
      let codified_arg = codify_local arg in
      Com.send loc currentEntity codified_arg  ^ _space ^ 
      _in ^ _endl ^ codified_thn
    | Rcv (Variable(name, typ), Location loc, thn, _) ->
      let codified_thn = codify thn confMap currentEntity in 
      let codified_arg = codify_local (Variable(name, typ)) in
      let local_type = ctrl_to_local_type typ "" in
        Com.rcv loc currentEntity codified_arg local_type ^ _space ^ _in ^ _space ^
        _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen 
    | Branch (ift, thn, el, _) -> 
      let codified_ift = codify ift confMap currentEntity in 
      let codified_thn = codify thn confMap currentEntity in
      let codified_el = codify el confMap currentEntity in
      _if ^ _space ^  codified_ift ^ _endl
      ^_then ^ _space ^  _lParen ^ codified_thn ^ _rParen ^ _endl ^ _else ^ 
      _space ^  _lParen ^ codified_el ^ _rParen
    | Choose (Direction d, Location loc, thn, _) -> 
      let codified_thn = codify thn confMap currentEntity in
      Com.send loc currentEntity (if d = "L" then "true" else "false")  ^ _space ^ 
      _in ^ _endl ^ _let ^ _space ^ _underscore ^ _space ^ _equals ^ _space ^ codified_thn ^ 
      _in ^ _space ^ _unit 
    | AllowL (_, _, _) ->
      raise (InvalidProgramException "AllowL not permitted.")
    | AllowR (_, _, _) ->
      raise (InvalidProgramException "AllowR not permitted.")
    | AllowLR (Location loc, thnL, thnR, _) ->
      let codified_thnL = codify thnL confMap currentEntity in
      let codified_thnR = codify thnR confMap currentEntity in
      Com.rcv loc currentEntity "___synclbl" "bool" ^ _space ^ _in ^ _space ^
      _if ^ _space ^ _lParen ^ _endl ^ "___synclbl" ^ _rParen ^ _then ^ _space ^ 
      _lParen ^ codified_thnL ^ _rParen ^ _endl ^ _else ^ _space ^ codified_thnR
    | Let (Unit, arg, thn, _) -> 
      let codified_arg = codify arg confMap currentEntity in
      let codified_thn = codify thn confMap currentEntity in 
        _let ^ _space ^ _underscore ^ _equals ^ codified_arg ^ _space ^ _in ^ 
        _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
    | Let (binder, arg, thn, typ) ->
    let codified_binder = codify binder confMap currentEntity in 
    let codified_arg = codify arg confMap currentEntity in
    let codified_thn = codify thn confMap currentEntity in 
      _let ^ _space ^ codified_binder ^ _colon ^ (ctrl_to_local_type typ "") ^ _equals ^ codified_arg ^ _space ^ _in ^ 
      _lParen ^ _endl ^ _tab ^ codified_thn ^ _rParen
    | Fun (Name name, ChoreoVars(ch_name, ch_typ), body, typ) ->
      let codified_arg = codify (ChoreoVars(ch_name, ch_typ)) confMap currentEntity in
      let codified_body = codify body confMap currentEntity in 
        _let ^ _space ^ _rec ^ _space ^ name ^ _space ^ _lParen ^ codified_arg ^ _colon ^ 
        (ctrl_to_local_type ch_typ "") ^_rParen ^ _space ^ _colon 
        ^ (ctrl_to_local_type typ "snd") ^ _equals ^ _space ^ 
        _lParen ^ codified_body ^ _rParen
    (* | Calling (Name name, arg, _) -> 
      let codified_arg = codify arg confMap currentEntity in
        name ^ _space ^ codified_arg  *)
    | Application (Fun (Name name, arg, body, fun_typ), argument, _) ->
      let codify_funct = codify (Fun (Name name, arg, body, fun_typ)) confMap currentEntity in
      let codified_argument = codify argument confMap currentEntity in 
        codify_funct ^ _endl ^ _in ^ _space ^ 
        _let ^ _space ^ _disreg ^ _equals ^ name ^ _space ^ codified_argument ^ _space ^ _in ^ _space
        ^ _unit
    | Application (_, _, _) -> ""
    | _ -> raise (InvalidProgramException "Invalid Program construct encountered")


  let format_and_save_code code output_file =
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

  let main asts confMap op_file_name =
    let code = _threadImport ^ _commonBp ^ _lParen ^
                Com.init confMap ^ 
                (List.map (fun ast -> match ast with | Ast{code; prop} -> 
                  _let ^ _space ^ prop ^ "() = " ^ 
                  _lParen ^ codify code confMap prop ^ _rParen ^ _space ^ _in ^ _space) asts 
                  |> String.concat "\n \n") ^ Com.exit confMap ^ _rParen in
    (* print_endline code  *)
    format_and_save_code code op_file_name

end

