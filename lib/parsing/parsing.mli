module Pos_info : sig
  type t =
    { fname : string
    ; start : int * int
    ; stop : int * int
    }
end

module Choreo : sig
  type 'a with_info = 'a * Pos_info.t
  type typ = Ast_core.Choreo.M.typ with_info
  type pattern = Ast_core.Choreo.M.pattern with_info
  type expr = Ast_core.Choreo.M.expr with_info
  type stmt = Ast_core.Choreo.M.stmt with_info
  type stmt_block = stmt list

  val info_of : 'a with_info -> Pos_info.t
end

val parse_program : Lexing.lexbuf -> Ast_core.Choreo.M.stmt_block
