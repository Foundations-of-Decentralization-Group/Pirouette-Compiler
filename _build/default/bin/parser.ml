
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RParen
    | Lambda
    | LParen
    | Identifier of (
# 5 "bin/parser.mly"
       (string)
# 18 "bin/parser.ml"
  )
    | EOF
    | END
    | Dot
  
end

include MenhirBasics

# 1 "bin/parser.mly"
  
    open Expr

# 32 "bin/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_prog) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState03 : (('s, _menhir_box_prog) _menhir_cell1_Lambda _menhir_cell0_Identifier, _menhir_box_prog) _menhir_state
    (** State 03.
        Stack shape : Lambda Identifier.
        Start symbol: prog. *)

  | MenhirState04 : (('s, _menhir_box_prog) _menhir_cell1_LParen, _menhir_box_prog) _menhir_state
    (** State 04.
        Stack shape : LParen.
        Start symbol: prog. *)

  | MenhirState10 : (('s, _menhir_box_prog) _menhir_cell1_application, _menhir_box_prog) _menhir_state
    (** State 10.
        Stack shape : application.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_application = 
  | MenhirCell1_application of 's * ('s, 'r) _menhir_state * (Expr.expr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Expr.expr)

and 's _menhir_cell0_Identifier = 
  | MenhirCell0_Identifier of 's * (
# 5 "bin/parser.mly"
       (string)
# 66 "bin/parser.ml"
)

and ('s, 'r) _menhir_cell1_LParen = 
  | MenhirCell1_LParen of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_Lambda = 
  | MenhirCell1_Lambda of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (Expr.expr option) [@@unboxed]

let _menhir_action_01 =
  fun _1 ->
    (_1 : (Expr.expr))

let _menhir_action_02 =
  fun a f ->
    (
# 37 "bin/parser.mly"
    (Application { funct = f; argument = a })
# 87 "bin/parser.ml"
     : (Expr.expr))

let _menhir_action_03 =
  fun _1 _3 e p ->
    (
# 32 "bin/parser.mly"
    ( Abstraction { param = p; body = e } )
# 95 "bin/parser.ml"
     : (Expr.expr))

let _menhir_action_04 =
  fun _1 ->
    (_1 : (Expr.expr))

let _menhir_action_05 =
  fun _1 ->
    (_1 : (unit))

let _menhir_action_06 =
  fun _1 ->
    (_1 : (unit))

let _menhir_action_07 =
  fun _1 ->
    (
# 20 "bin/parser.mly"
           (None)
# 115 "bin/parser.ml"
     : (Expr.expr option))

let _menhir_action_08 =
  fun _2 e ->
    (
# 21 "bin/parser.mly"
                          (Some e)
# 123 "bin/parser.ml"
     : (Expr.expr option))

let _menhir_action_09 =
  fun _1 ->
    (_1 : (Expr.expr))

let _menhir_action_10 =
  fun _1 _3 e ->
    (
# 28 "bin/parser.mly"
                                (e)
# 135 "bin/parser.ml"
     : (Expr.expr))

let _menhir_action_11 =
  fun i ->
    (
# 24 "bin/parser.mly"
                      (Variable i)
# 143 "bin/parser.ml"
     : (Expr.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | Dot ->
        "Dot"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | Identifier _ ->
        "Identifier"
    | LParen ->
        "LParen"
    | Lambda ->
        "Lambda"
    | RParen ->
        "RParen"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_goto_line_end : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_expr -> _ -> _menhir_box_prog =
    fun _menhir_stack _v ->
      let MenhirCell1_expr (_menhir_stack, _, e) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_08 _2 e in
      MenhirBox_prog _v
  
  let rec _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = () in
          let _v = _menhir_action_06 _1 in
          _menhir_goto_line_end _menhir_stack _v
      | END ->
          let _1 = () in
          let _v = _menhir_action_05 _1 in
          _menhir_goto_line_end _menhir_stack _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_Lambda (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Identifier _v ->
          let _menhir_stack = MenhirCell0_Identifier (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | Dot ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | Lambda ->
                  _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
              | LParen ->
                  _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
              | Identifier _v_0 ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v_1 =
                    let i = _v_0 in
                    _menhir_action_11 i
                  in
                  let _1 = _v_1 in
                  let _v = _menhir_action_09 _1 in
                  _menhir_run_07_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LParen (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lambda ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | LParen ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | Identifier _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let i = _v in
            _menhir_action_11 i
          in
          let _1 = _v in
          let _v = _menhir_action_09 _1 in
          _menhir_run_07_spec_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07_spec_04 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LParen -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LParen ->
          let _menhir_stack = MenhirCell1_application (_menhir_stack, _menhir_s, _v) in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | Identifier _v_0 ->
          let _menhir_stack = MenhirCell1_application (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 =
            let i = _v_0 in
            _menhir_action_11 i
          in
          let _1 = _v_1 in
          let _v = _menhir_action_09 _1 in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | END | EOF | RParen ->
          let _1 = _v in
          let _v = _menhir_action_04 _1 in
          _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_application -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_application (_menhir_stack, _menhir_s, f) = _menhir_stack in
      let a = _v in
      let _v = _menhir_action_02 a f in
      _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_15 _menhir_stack _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_Lambda _menhir_cell0_Identifier -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_Identifier (_menhir_stack, p) = _menhir_stack in
      let MenhirCell1_Lambda (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_1, e, _3) = ((), _v, ()) in
      let _v = _menhir_action_03 _1 _3 e p in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LParen -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RParen ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LParen (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_1, e, _3) = ((), _v, ()) in
          let _v = _menhir_action_10 _1 _3 e in
          _menhir_goto_sub_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_sub_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_07_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_07_spec_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_07_spec_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_07_spec_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
  
  and _menhir_run_07_spec_03 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_Lambda _menhir_cell0_Identifier -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_01 _1 in
      _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | Lambda ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LParen ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | Identifier _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let i = _v in
            _menhir_action_11 i
          in
          let _1 = _v in
          let _v = _menhir_action_09 _1 in
          _menhir_run_07_spec_00 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | EOF ->
          let _v =
            let _1 = () in
            _menhir_action_07 _1
          in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
