
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WITH
    | UNIT_T
    | UNDERSCORE
    | TYPE
    | TRUE
    | TIMES
    | TILDE_ARROW
    | THEN
    | STRING_T
    | STRING of (
# 3 "lib/parsing/net_parser.mly"
       (string)
# 24 "lib/parsing/net_parser.ml"
  )
    | SND
    | SEND
    | SEMICOLON
    | RPAREN
    | RIGHT
    | RET
    | RECV
    | PLUS
    | OR
    | NOT
    | NEQ
    | MINUS
    | MATCH
    | LT
    | LPAREN
    | LET
    | LEQ
    | LEFT
    | INT_T
    | INT of (
# 2 "lib/parsing/net_parser.mly"
       (int)
# 48 "lib/parsing/net_parser.ml"
  )
    | IN
    | IF
    | ID of (
# 1 "lib/parsing/net_parser.mly"
       (string)
# 55 "lib/parsing/net_parser.ml"
  )
    | GT
    | GEQ
    | FUN
    | FST
    | FROM
    | FOREIGN
    | FOR
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DOT
    | DIV
    | COMMA
    | COLONEQ
    | COLON
    | CHOOSE
    | CHOICE
    | BOOL_T
    | BAR
    | ARROW
    | AND
    | ALLOW
  
end

include MenhirBasics

# 41 "lib/parsing/net_parser.mly"
  
  open Ast_core.Local.M
  open Ast_core.Net.M
  open Parsed_ast

  let gen_pos startpos endpos =
    let open Lexing in
    { Pos_info.fname = startpos.pos_fname
    ; start = startpos.pos_lnum, startpos.pos_cnum - startpos.pos_bol
    ; stop = endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol
    }

# 98 "lib/parsing/net_parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_prog) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState005 : (('s, _menhir_box_prog) _menhir_cell1_TYPE _menhir_cell0_typ_id, _menhir_box_prog) _menhir_state
    (** State 005.
        Stack shape : TYPE typ_id.
        Start symbol: prog. *)

  | MenhirState007 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 007.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState010 : (('s, _menhir_box_prog) _menhir_cell1_net_type _menhir_cell0_TIMES, _menhir_box_prog) _menhir_state
    (** State 010.
        Stack shape : net_type TIMES.
        Start symbol: prog. *)

  | MenhirState013 : (('s, _menhir_box_prog) _menhir_cell1_loc_id, _menhir_box_prog) _menhir_state
    (** State 013.
        Stack shape : loc_id.
        Start symbol: prog. *)

  | MenhirState016 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 016.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState020 : (('s, _menhir_box_prog) _menhir_cell1_local_type _menhir_cell0_TIMES, _menhir_box_prog) _menhir_state
    (** State 020.
        Stack shape : local_type TIMES.
        Start symbol: prog. *)

  | MenhirState023 : (('s, _menhir_box_prog) _menhir_cell1_local_type _menhir_cell0_PLUS, _menhir_box_prog) _menhir_state
    (** State 023.
        Stack shape : local_type PLUS.
        Start symbol: prog. *)

  | MenhirState027 : (('s, _menhir_box_prog) _menhir_cell1_net_type _menhir_cell0_PLUS, _menhir_box_prog) _menhir_state
    (** State 027.
        Stack shape : net_type PLUS.
        Start symbol: prog. *)

  | MenhirState029 : (('s, _menhir_box_prog) _menhir_cell1_net_type, _menhir_box_prog) _menhir_state
    (** State 029.
        Stack shape : net_type.
        Start symbol: prog. *)

  | MenhirState036 : (('s, _menhir_box_prog) _menhir_cell1_RIGHT, _menhir_box_prog) _menhir_state
    (** State 036.
        Stack shape : RIGHT.
        Start symbol: prog. *)

  | MenhirState037 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 037.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState038 : (('s, _menhir_box_prog) _menhir_cell1_LEFT, _menhir_box_prog) _menhir_state
    (** State 038.
        Stack shape : LEFT.
        Start symbol: prog. *)

  | MenhirState047 : ((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_state
    (** State 047.
        Stack shape : LPAREN local_pattern.
        Start symbol: prog. *)

  | MenhirState051 : (('s, _menhir_box_prog) _menhir_cell1_FOREIGN, _menhir_box_prog) _menhir_state
    (** State 051.
        Stack shape : FOREIGN.
        Start symbol: prog. *)

  | MenhirState053 : ((('s, _menhir_box_prog) _menhir_cell1_FOREIGN, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_state
    (** State 053.
        Stack shape : FOREIGN var_id.
        Start symbol: prog. *)

  | MenhirState060 : (('s, _menhir_box_prog) _menhir_cell1_stmt, _menhir_box_prog) _menhir_state
    (** State 060.
        Stack shape : stmt.
        Start symbol: prog. *)

  | MenhirState062 : (('s, _menhir_box_prog) _menhir_cell1_nonempty_list_local_pattern_, _menhir_box_prog) _menhir_state
    (** State 062.
        Stack shape : nonempty_list(local_pattern).
        Start symbol: prog. *)

  | MenhirState064 : (('s, _menhir_box_prog) _menhir_cell1_SND, _menhir_box_prog) _menhir_state
    (** State 064.
        Stack shape : SND.
        Start symbol: prog. *)

  | MenhirState065 : (('s, _menhir_box_prog) _menhir_cell1_SEND, _menhir_box_prog) _menhir_state
    (** State 065.
        Stack shape : SEND.
        Start symbol: prog. *)

  | MenhirState066 : (('s, _menhir_box_prog) _menhir_cell1_RIGHT, _menhir_box_prog) _menhir_state
    (** State 066.
        Stack shape : RIGHT.
        Start symbol: prog. *)

  | MenhirState067 : (('s, _menhir_box_prog) _menhir_cell1_RET, _menhir_box_prog) _menhir_state
    (** State 067.
        Stack shape : RET.
        Start symbol: prog. *)

  | MenhirState068 : (('s, _menhir_box_prog) _menhir_cell1_SND, _menhir_box_prog) _menhir_state
    (** State 068.
        Stack shape : SND.
        Start symbol: prog. *)

  | MenhirState069 : (('s, _menhir_box_prog) _menhir_cell1_RIGHT, _menhir_box_prog) _menhir_state
    (** State 069.
        Stack shape : RIGHT.
        Start symbol: prog. *)

  | MenhirState070 : (('s, _menhir_box_prog) _menhir_cell1_NOT, _menhir_box_prog) _menhir_state
    (** State 070.
        Stack shape : NOT.
        Start symbol: prog. *)

  | MenhirState071 : (('s, _menhir_box_prog) _menhir_cell1_MINUS, _menhir_box_prog) _menhir_state
    (** State 071.
        Stack shape : MINUS.
        Start symbol: prog. *)

  | MenhirState072 : (('s, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_state
    (** State 072.
        Stack shape : MATCH.
        Start symbol: prog. *)

  | MenhirState073 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 073.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState075 : (('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_state
    (** State 075.
        Stack shape : LET.
        Start symbol: prog. *)

  | MenhirState077 : ((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_state
    (** State 077.
        Stack shape : LET var_id.
        Start symbol: prog. *)

  | MenhirState079 : (((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type, _menhir_box_prog) _menhir_state
    (** State 079.
        Stack shape : LET var_id local_type.
        Start symbol: prog. *)

  | MenhirState080 : (('s, _menhir_box_prog) _menhir_cell1_LEFT, _menhir_box_prog) _menhir_state
    (** State 080.
        Stack shape : LEFT.
        Start symbol: prog. *)

  | MenhirState081 : (('s, _menhir_box_prog) _menhir_cell1_FST, _menhir_box_prog) _menhir_state
    (** State 081.
        Stack shape : FST.
        Start symbol: prog. *)

  | MenhirState084 : ((('s, _menhir_box_prog) _menhir_cell1_FST, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 084.
        Stack shape : FST local_expr.
        Start symbol: prog. *)

  | MenhirState085 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_TIMES, _menhir_box_prog) _menhir_state
    (** State 085.
        Stack shape : local_expr TIMES.
        Start symbol: prog. *)

  | MenhirState087 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_PLUS, _menhir_box_prog) _menhir_state
    (** State 087.
        Stack shape : local_expr PLUS.
        Start symbol: prog. *)

  | MenhirState088 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_PLUS, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 088.
        Stack shape : local_expr PLUS local_expr.
        Start symbol: prog. *)

  | MenhirState089 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_DIV, _menhir_box_prog) _menhir_state
    (** State 089.
        Stack shape : local_expr DIV.
        Start symbol: prog. *)

  | MenhirState091 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_OR, _menhir_box_prog) _menhir_state
    (** State 091.
        Stack shape : local_expr OR.
        Start symbol: prog. *)

  | MenhirState092 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_OR, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 092.
        Stack shape : local_expr OR local_expr.
        Start symbol: prog. *)

  | MenhirState093 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_NEQ, _menhir_box_prog) _menhir_state
    (** State 093.
        Stack shape : local_expr NEQ.
        Start symbol: prog. *)

  | MenhirState094 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_NEQ, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 094.
        Stack shape : local_expr NEQ local_expr.
        Start symbol: prog. *)

  | MenhirState095 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_MINUS, _menhir_box_prog) _menhir_state
    (** State 095.
        Stack shape : local_expr MINUS.
        Start symbol: prog. *)

  | MenhirState096 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_MINUS, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 096.
        Stack shape : local_expr MINUS local_expr.
        Start symbol: prog. *)

  | MenhirState097 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LT, _menhir_box_prog) _menhir_state
    (** State 097.
        Stack shape : local_expr LT.
        Start symbol: prog. *)

  | MenhirState098 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LT, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 098.
        Stack shape : local_expr LT local_expr.
        Start symbol: prog. *)

  | MenhirState099 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LEQ, _menhir_box_prog) _menhir_state
    (** State 099.
        Stack shape : local_expr LEQ.
        Start symbol: prog. *)

  | MenhirState100 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LEQ, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 100.
        Stack shape : local_expr LEQ local_expr.
        Start symbol: prog. *)

  | MenhirState101 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GT, _menhir_box_prog) _menhir_state
    (** State 101.
        Stack shape : local_expr GT.
        Start symbol: prog. *)

  | MenhirState102 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GT, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 102.
        Stack shape : local_expr GT local_expr.
        Start symbol: prog. *)

  | MenhirState103 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GEQ, _menhir_box_prog) _menhir_state
    (** State 103.
        Stack shape : local_expr GEQ.
        Start symbol: prog. *)

  | MenhirState104 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GEQ, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 104.
        Stack shape : local_expr GEQ local_expr.
        Start symbol: prog. *)

  | MenhirState105 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_EQ, _menhir_box_prog) _menhir_state
    (** State 105.
        Stack shape : local_expr EQ.
        Start symbol: prog. *)

  | MenhirState106 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_EQ, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 106.
        Stack shape : local_expr EQ local_expr.
        Start symbol: prog. *)

  | MenhirState107 : ((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_AND, _menhir_box_prog) _menhir_state
    (** State 107.
        Stack shape : local_expr AND.
        Start symbol: prog. *)

  | MenhirState108 : (((('s, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_AND, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 108.
        Stack shape : local_expr AND local_expr.
        Start symbol: prog. *)

  | MenhirState109 : ((('s, _menhir_box_prog) _menhir_cell1_LEFT, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 109.
        Stack shape : LEFT local_expr.
        Start symbol: prog. *)

  | MenhirState110 : ((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 110.
        Stack shape : LET var_id local_type local_expr.
        Start symbol: prog. *)

  | MenhirState111 : (((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_IN, _menhir_box_prog) _menhir_state
    (** State 111.
        Stack shape : LET var_id local_type local_expr IN.
        Start symbol: prog. *)

  | MenhirState112 : ((((((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_IN, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 112.
        Stack shape : LET var_id local_type local_expr IN local_expr.
        Start symbol: prog. *)

  | MenhirState113 : ((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 113.
        Stack shape : LPAREN local_expr.
        Start symbol: prog. *)

  | MenhirState115 : (((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_COMMA, _menhir_box_prog) _menhir_state
    (** State 115.
        Stack shape : LPAREN local_expr COMMA.
        Start symbol: prog. *)

  | MenhirState116 : ((((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_COMMA, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 116.
        Stack shape : LPAREN local_expr COMMA local_expr.
        Start symbol: prog. *)

  | MenhirState118 : ((('s, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 118.
        Stack shape : MATCH local_expr.
        Start symbol: prog. *)

  | MenhirState119 : (((('s, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_WITH, _menhir_box_prog) _menhir_state
    (** State 119.
        Stack shape : MATCH local_expr WITH.
        Start symbol: prog. *)

  | MenhirState120 : (('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_state
    (** State 120.
        Stack shape : BAR.
        Start symbol: prog. *)

  | MenhirState122 : ((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_state
    (** State 122.
        Stack shape : BAR local_pattern.
        Start symbol: prog. *)

  | MenhirState123 : (((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 123.
        Stack shape : BAR local_pattern local_expr.
        Start symbol: prog. *)

  | MenhirState128 : ((('s, _menhir_box_prog) _menhir_cell1_RIGHT, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 128.
        Stack shape : RIGHT local_expr.
        Start symbol: prog. *)

  | MenhirState129 : ((('s, _menhir_box_prog) _menhir_cell1_SND, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 129.
        Stack shape : SND local_expr.
        Start symbol: prog. *)

  | MenhirState130 : ((('s, _menhir_box_prog) _menhir_cell1_RET, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_state
    (** State 130.
        Stack shape : RET local_expr.
        Start symbol: prog. *)

  | MenhirState132 : (('s, _menhir_box_prog) _menhir_cell1_RECV, _menhir_box_prog) _menhir_state
    (** State 132.
        Stack shape : RECV.
        Start symbol: prog. *)

  | MenhirState134 : (('s, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_state
    (** State 134.
        Stack shape : MATCH.
        Start symbol: prog. *)

  | MenhirState135 : (('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_state
    (** State 135.
        Stack shape : LPAREN.
        Start symbol: prog. *)

  | MenhirState136 : (('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_state
    (** State 136.
        Stack shape : LET.
        Start symbol: prog. *)

  | MenhirState138 : ((('s, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_stmt_block, _menhir_box_prog) _menhir_state
    (** State 138.
        Stack shape : LET stmt_block.
        Start symbol: prog. *)

  | MenhirState139 : (('s, _menhir_box_prog) _menhir_cell1_LEFT, _menhir_box_prog) _menhir_state
    (** State 139.
        Stack shape : LEFT.
        Start symbol: prog. *)

  | MenhirState140 : (('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_state
    (** State 140.
        Stack shape : IF.
        Start symbol: prog. *)

  | MenhirState141 : (('s, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_state
    (** State 141.
        Stack shape : FUN.
        Start symbol: prog. *)

  | MenhirState143 : ((('s, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_cell1_nonempty_list_local_pattern_, _menhir_box_prog) _menhir_state
    (** State 143.
        Stack shape : FUN nonempty_list(local_pattern).
        Start symbol: prog. *)

  | MenhirState144 : (('s, _menhir_box_prog) _menhir_cell1_FST, _menhir_box_prog) _menhir_state
    (** State 144.
        Stack shape : FST.
        Start symbol: prog. *)

  | MenhirState145 : (('s, _menhir_box_prog) _menhir_cell1_CHOOSE, _menhir_box_prog) _menhir_state
    (** State 145.
        Stack shape : CHOOSE.
        Start symbol: prog. *)

  | MenhirState148 : ((('s, _menhir_box_prog) _menhir_cell1_CHOOSE, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_state
    (** State 148.
        Stack shape : CHOOSE sync_label.
        Start symbol: prog. *)

  | MenhirState150 : (((('s, _menhir_box_prog) _menhir_cell1_CHOOSE, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_cell1_loc_id, _menhir_box_prog) _menhir_state
    (** State 150.
        Stack shape : CHOOSE sync_label loc_id.
        Start symbol: prog. *)

  | MenhirState153 : (('s, _menhir_box_prog) _menhir_cell1_ALLOW, _menhir_box_prog) _menhir_state
    (** State 153.
        Stack shape : ALLOW.
        Start symbol: prog. *)

  | MenhirState155 : ((('s, _menhir_box_prog) _menhir_cell1_ALLOW, _menhir_box_prog) _menhir_cell1_loc_id, _menhir_box_prog) _menhir_state
    (** State 155.
        Stack shape : ALLOW loc_id.
        Start symbol: prog. *)

  | MenhirState156 : (('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_state
    (** State 156.
        Stack shape : BAR.
        Start symbol: prog. *)

  | MenhirState158 : ((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_state
    (** State 158.
        Stack shape : BAR sync_label.
        Start symbol: prog. *)

  | MenhirState160 : (((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 160.
        Stack shape : BAR sync_label net_expr.
        Start symbol: prog. *)

  | MenhirState166 : (('s, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_state
    (** State 166.
        Stack shape : local_pattern.
        Start symbol: prog. *)

  | MenhirState169 : ((('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 169.
        Stack shape : IF net_expr.
        Start symbol: prog. *)

  | MenhirState171 : (((('s, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 171.
        Stack shape : IF net_expr net_expr.
        Start symbol: prog. *)

  | MenhirState175 : (('s, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_state
    (** State 175.
        Stack shape : local_pattern.
        Start symbol: prog. *)

  | MenhirState176 : ((('s, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_COLON, _menhir_box_prog) _menhir_state
    (** State 176.
        Stack shape : local_pattern COLON.
        Start symbol: prog. *)

  | MenhirState183 : ((('s, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 183.
        Stack shape : LPAREN net_expr.
        Start symbol: prog. *)

  | MenhirState187 : ((('s, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 187.
        Stack shape : MATCH net_expr.
        Start symbol: prog. *)

  | MenhirState188 : (('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_state
    (** State 188.
        Stack shape : BAR.
        Start symbol: prog. *)

  | MenhirState190 : ((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_state
    (** State 190.
        Stack shape : BAR local_pattern.
        Start symbol: prog. *)

  | MenhirState191 : (((('s, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 191.
        Stack shape : BAR local_pattern net_expr.
        Start symbol: prog. *)

  | MenhirState196 : ((('s, _menhir_box_prog) _menhir_cell1_SEND, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_state
    (** State 196.
        Stack shape : SEND net_expr.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_loc_id = 
  | MenhirCell1_loc_id of 's * ('s, 'r) _menhir_state * (
# 64 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.loc_id)
# 606 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_local_expr = 
  | MenhirCell1_local_expr of 's * ('s, 'r) _menhir_state * (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 613 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_local_pattern = 
  | MenhirCell1_local_pattern of 's * ('s, 'r) _menhir_state * (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 620 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_local_type = 
  | MenhirCell1_local_type of 's * ('s, 'r) _menhir_state * (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 627 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_net_expr = 
  | MenhirCell1_net_expr of 's * ('s, 'r) _menhir_state * (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 634 "lib/parsing/net_parser.ml"
) * Lexing.position

and ('s, 'r) _menhir_cell1_net_type = 
  | MenhirCell1_net_type of 's * ('s, 'r) _menhir_state * (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 641 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_nonempty_list_local_pattern_ = 
  | MenhirCell1_nonempty_list_local_pattern_ of 's * ('s, 'r) _menhir_state * (
# 71 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern list)
# 648 "lib/parsing/net_parser.ml"
) * Lexing.position

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (
# 56 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 655 "lib/parsing/net_parser.ml"
)

and ('s, 'r) _menhir_cell1_stmt_block = 
  | MenhirCell1_stmt_block of 's * ('s, 'r) _menhir_state * (
# 55 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt_block)
# 662 "lib/parsing/net_parser.ml"
)

and ('s, 'r) _menhir_cell1_sync_label = 
  | MenhirCell1_sync_label of 's * ('s, 'r) _menhir_state * (
# 67 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.sync_label)
# 669 "lib/parsing/net_parser.ml"
)

and 's _menhir_cell0_typ_id = 
  | MenhirCell0_typ_id of 's * (
# 66 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ_id)
# 676 "lib/parsing/net_parser.ml"
)

and ('s, 'r) _menhir_cell1_var_id = 
  | MenhirCell1_var_id of 's * ('s, 'r) _menhir_state * (
# 65 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.var_id)
# 683 "lib/parsing/net_parser.ml"
) * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_ALLOW = 
  | MenhirCell1_ALLOW of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_AND = 
  | MenhirCell1_AND of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_BAR = 
  | MenhirCell1_BAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CHOOSE = 
  | MenhirCell1_CHOOSE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_COLON = 
  | MenhirCell1_COLON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA = 
  | MenhirCell1_COMMA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DIV = 
  | MenhirCell1_DIV of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_FOREIGN = 
  | MenhirCell1_FOREIGN of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_FST = 
  | MenhirCell1_FST of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_FUN = 
  | MenhirCell1_FUN of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_GEQ = 
  | MenhirCell1_GEQ of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_GT = 
  | MenhirCell1_GT of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_IN = 
  | MenhirCell1_IN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LEFT = 
  | MenhirCell1_LEFT of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LEQ = 
  | MenhirCell1_LEQ of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_LET = 
  | MenhirCell1_LET of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_LT = 
  | MenhirCell1_LT of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_MATCH = 
  | MenhirCell1_MATCH of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_NEQ = 
  | MenhirCell1_NEQ of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_OR = 
  | MenhirCell1_OR of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and 's _menhir_cell0_PLUS = 
  | MenhirCell0_PLUS of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_RECV = 
  | MenhirCell1_RECV of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_RET = 
  | MenhirCell1_RET of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_RIGHT = 
  | MenhirCell1_RIGHT of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_SEND = 
  | MenhirCell1_SEND of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_SND = 
  | MenhirCell1_SND of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_TIMES = 
  | MenhirCell1_TIMES of 's * ('s, 'r) _menhir_state * Lexing.position * Lexing.position

and 's _menhir_cell0_TIMES = 
  | MenhirCell0_TIMES of 's * Lexing.position * Lexing.position

and ('s, 'r) _menhir_cell1_TYPE = 
  | MenhirCell1_TYPE of 's * ('s, 'r) _menhir_state * Lexing.position

and ('s, 'r) _menhir_cell1_WITH = 
  | MenhirCell1_WITH of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (
# 54 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt_block)
# 798 "lib/parsing/net_parser.ml"
) [@@unboxed]

let _menhir_action_01 =
  fun _endpos__7_ _startpos__1_ id s t ->
    let _endpos = _endpos__7_ in
    let _startpos = _startpos__1_ in
    (
# 200 "lib/parsing/net_parser.mly"
    ( ForeignDecl (id, t, s, gen_pos _startpos _endpos) )
# 808 "lib/parsing/net_parser.ml"
     : (
# 68 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 812 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_02 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 820 "lib/parsing/net_parser.ml"
     : (
# 69 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt list)
# 824 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_03 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 832 "lib/parsing/net_parser.ml"
     : (
# 69 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt list)
# 836 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_04 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 154 "lib/parsing/net_parser.mly"
          ( LocId (id, gen_pos _startpos _endpos) )
# 846 "lib/parsing/net_parser.ml"
     : (
# 64 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.loc_id)
# 850 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_05 =
  fun _endpos__2_ _startpos__1_ ->
    let _endpos = _endpos__2_ in
    let _startpos = _startpos__1_ in
    (
# 113 "lib/parsing/net_parser.mly"
                  ( Unit (gen_pos _startpos _endpos) )
# 860 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 864 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_06 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    (
# 114 "lib/parsing/net_parser.mly"
            ( Val (v, gen_pos _startpos _endpos) )
# 874 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 878 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_07 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 115 "lib/parsing/net_parser.mly"
              ( Var (id, gen_pos _startpos _endpos) )
# 888 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 892 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_08 =
  fun _endpos__1_ _endpos_e_ _startpos__1_ e ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 181 "lib/parsing/net_parser.mly"
          ( Neg (gen_pos _startpos _endpos) )
# 903 "lib/parsing/net_parser.ml"
      
    in
    let _startpos_op_ = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_op_ in
    (
# 116 "lib/parsing/net_parser.mly"
                                      ( UnOp (op, e, gen_pos _startpos _endpos) )
# 912 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 916 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_09 =
  fun _endpos__1_ _endpos_e_ _startpos__1_ e ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 182 "lib/parsing/net_parser.mly"
        ( Not (gen_pos _startpos _endpos) )
# 927 "lib/parsing/net_parser.ml"
      
    in
    let _startpos_op_ = _startpos__1_ in
    let _endpos = _endpos_e_ in
    let _startpos = _startpos_op_ in
    (
# 116 "lib/parsing/net_parser.mly"
                                      ( UnOp (op, e, gen_pos _startpos _endpos) )
# 936 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 940 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_10 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 185 "lib/parsing/net_parser.mly"
         ( Plus (gen_pos _startpos _endpos) )
# 951 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 959 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 963 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_11 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 186 "lib/parsing/net_parser.mly"
          ( Minus (gen_pos _startpos _endpos) )
# 974 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 982 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 986 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_12 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 187 "lib/parsing/net_parser.mly"
          ( Times (gen_pos _startpos _endpos) )
# 997 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1005 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1009 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_13 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 188 "lib/parsing/net_parser.mly"
        ( Div (gen_pos _startpos _endpos) )
# 1020 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1028 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1032 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_14 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 189 "lib/parsing/net_parser.mly"
        ( And (gen_pos _startpos _endpos) )
# 1043 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1051 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1055 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_15 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 190 "lib/parsing/net_parser.mly"
       ( Or (gen_pos _startpos _endpos) )
# 1066 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1074 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1078 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_16 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 191 "lib/parsing/net_parser.mly"
       ( Eq (gen_pos _startpos _endpos) )
# 1089 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1097 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1101 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_17 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 192 "lib/parsing/net_parser.mly"
        ( Neq (gen_pos _startpos _endpos) )
# 1112 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1120 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1124 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_18 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 193 "lib/parsing/net_parser.mly"
       ( Lt (gen_pos _startpos _endpos) )
# 1135 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1143 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1147 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_19 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 194 "lib/parsing/net_parser.mly"
        ( Leq (gen_pos _startpos _endpos) )
# 1158 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1166 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1170 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_20 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 195 "lib/parsing/net_parser.mly"
       ( Gt (gen_pos _startpos _endpos) )
# 1181 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1189 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1193 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_21 =
  fun _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 ->
    let op =
      let _endpos = _endpos__1_ in
      let _startpos = _startpos__1_ in
      
# 196 "lib/parsing/net_parser.mly"
        ( Geq (gen_pos _startpos _endpos) )
# 1204 "lib/parsing/net_parser.ml"
      
    in
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos_e1_ in
    (
# 117 "lib/parsing/net_parser.mly"
                                          ( BinOp (e1, op, e2, gen_pos _startpos _endpos) )
# 1212 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1216 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_22 =
  fun _endpos_e2_ _startpos__1_ e1 e2 id t ->
    let _endpos = _endpos_e2_ in
    let _startpos = _startpos__1_ in
    (
# 118 "lib/parsing/net_parser.mly"
                                                                            ( Let (id, t, e1, e2, gen_pos _startpos _endpos) )
# 1226 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1230 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_23 =
  fun _endpos__5_ _startpos__1_ e1 e2 ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 119 "lib/parsing/net_parser.mly"
                                                    ( Pair (e1, e2, gen_pos _startpos _endpos) )
# 1240 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1244 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_24 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 120 "lib/parsing/net_parser.mly"
                     ( Fst (e, gen_pos _startpos _endpos) )
# 1254 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1258 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_25 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 121 "lib/parsing/net_parser.mly"
                     ( Snd (e, gen_pos _startpos _endpos) )
# 1268 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1272 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_26 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 122 "lib/parsing/net_parser.mly"
                      ( Left (e, gen_pos _startpos _endpos) )
# 1282 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1286 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_27 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 123 "lib/parsing/net_parser.mly"
                       ( Right (e, gen_pos _startpos _endpos) )
# 1296 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1300 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_28 =
  fun _endpos_cases_ _startpos__1_ cases e ->
    let _endpos = _endpos_cases_ in
    let _startpos = _startpos__1_ in
    (
# 124 "lib/parsing/net_parser.mly"
                                                            ( Match (e, cases, gen_pos _startpos _endpos) )
# 1310 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1314 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_29 =
  fun _endpos__3_ _startpos__1_ e ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 125 "lib/parsing/net_parser.mly"
                               ( Local.set_info_expr (gen_pos _startpos _endpos) e )
# 1324 "lib/parsing/net_parser.ml"
     : (
# 59 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.expr)
# 1328 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_30 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 128 "lib/parsing/net_parser.mly"
               ( Default (gen_pos _startpos _endpos) )
# 1338 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1342 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_31 =
  fun _endpos_v_ _startpos_v_ v ->
    let _endpos = _endpos_v_ in
    let _startpos = _startpos_v_ in
    (
# 129 "lib/parsing/net_parser.mly"
            ( Val (v, gen_pos _startpos _endpos) )
# 1352 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1356 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_32 =
  fun _endpos_x_ _startpos_x_ x ->
    let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    (
# 130 "lib/parsing/net_parser.mly"
             ( Var (x, gen_pos _startpos _endpos) )
# 1366 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1370 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_33 =
  fun _endpos__5_ _startpos__1_ p1 p2 ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 131 "lib/parsing/net_parser.mly"
                                                          ( Pair (p1, p2, gen_pos _startpos _endpos) )
# 1380 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1384 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_34 =
  fun _endpos_p_ _startpos__1_ p ->
    let _endpos = _endpos_p_ in
    let _startpos = _startpos__1_ in
    (
# 132 "lib/parsing/net_parser.mly"
                         ( Left (p, gen_pos _startpos _endpos) )
# 1394 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1398 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_35 =
  fun _endpos_p_ _startpos__1_ p ->
    let _endpos = _endpos_p_ in
    let _startpos = _startpos__1_ in
    (
# 133 "lib/parsing/net_parser.mly"
                          ( Right (p, gen_pos _startpos _endpos) )
# 1408 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1412 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_36 =
  fun _endpos__3_ _startpos__1_ p ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 134 "lib/parsing/net_parser.mly"
                                  ( Local.set_info_pattern (gen_pos _startpos _endpos) p )
# 1422 "lib/parsing/net_parser.ml"
     : (
# 60 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern)
# 1426 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_37 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 145 "lib/parsing/net_parser.mly"
           ( TUnit (gen_pos _startpos _endpos) )
# 1436 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1440 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_38 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 146 "lib/parsing/net_parser.mly"
          ( TInt (gen_pos _startpos _endpos) )
# 1450 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1454 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_39 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 147 "lib/parsing/net_parser.mly"
             ( TString (gen_pos _startpos _endpos) )
# 1464 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1468 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_40 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 148 "lib/parsing/net_parser.mly"
           ( TBool (gen_pos _startpos _endpos) )
# 1478 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1482 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_41 =
  fun _endpos_t2_ _startpos_t1_ t1 t2 ->
    let _endpos = _endpos_t2_ in
    let _startpos = _startpos_t1_ in
    (
# 149 "lib/parsing/net_parser.mly"
                                      ( TProd (t1, t2, gen_pos _startpos _endpos) )
# 1492 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1496 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_42 =
  fun _endpos_t2_ _startpos_t1_ t1 t2 ->
    let _endpos = _endpos_t2_ in
    let _startpos = _startpos_t1_ in
    (
# 150 "lib/parsing/net_parser.mly"
                                     ( TSum (t1, t2, gen_pos _startpos _endpos) )
# 1506 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1510 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_43 =
  fun _endpos__3_ _startpos__1_ t ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 151 "lib/parsing/net_parser.mly"
                               ( Local.set_info_typ (gen_pos _startpos _endpos) t )
# 1520 "lib/parsing/net_parser.ml"
     : (
# 61 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ)
# 1524 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_44 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 93 "lib/parsing/net_parser.mly"
           ( Unit (gen_pos _startpos _endpos) )
# 1534 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1538 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_45 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 94 "lib/parsing/net_parser.mly"
              ( Var (id, gen_pos _startpos _endpos) )
# 1548 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1552 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_46 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 95 "lib/parsing/net_parser.mly"
                     ( Ret (e, gen_pos _startpos _endpos) )
# 1562 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1566 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_47 =
  fun _endpos_e3_ _startpos__1_ e1 e2 e3 ->
    let _endpos = _endpos_e3_ in
    let _startpos = _startpos__1_ in
    (
# 96 "lib/parsing/net_parser.mly"
                                                     ( If (e1, e2, e3, gen_pos _startpos _endpos) )
# 1576 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1580 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_48 =
  fun _endpos_e_ _startpos__1_ e stmts ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 97 "lib/parsing/net_parser.mly"
                                       ( Let (stmts, e, gen_pos _startpos _endpos) )
# 1590 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1594 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_49 =
  fun _endpos_id_ _startpos__1_ e id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos__1_ in
    (
# 98 "lib/parsing/net_parser.mly"
                                          ( Send (e, id, gen_pos _startpos _endpos) )
# 1604 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1608 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_50 =
  fun _endpos_id_ _startpos__1_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos__1_ in
    (
# 99 "lib/parsing/net_parser.mly"
                        ( Recv (id, gen_pos _startpos _endpos) )
# 1618 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1622 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_51 =
  fun _endpos_e_ _startpos__1_ e id sync ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 100 "lib/parsing/net_parser.mly"
                                                       ( ChooseFor (sync, id, e, gen_pos _startpos _endpos) )
# 1632 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1636 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_52 =
  fun _endpos_cases_ _startpos__1_ cases id ->
    let _endpos = _endpos_cases_ in
    let _startpos = _startpos__1_ in
    (
# 101 "lib/parsing/net_parser.mly"
                                                                           ( AllowChoice (id, cases, gen_pos _startpos _endpos) )
# 1646 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1650 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_53 =
  fun _endpos_e_ _startpos__1_ e ps ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 102 "lib/parsing/net_parser.mly"
                                                         ( FunDef (ps, e, gen_pos _startpos _endpos) )
# 1660 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1664 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_54 =
  fun _endpos__5_ _startpos__1_ e1 e2 ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 103 "lib/parsing/net_parser.mly"
                                                ( Pair (e1, e2, gen_pos _startpos _endpos) )
# 1674 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1678 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_55 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 104 "lib/parsing/net_parser.mly"
                   ( Fst (e, gen_pos _startpos _endpos) )
# 1688 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1692 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_56 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 105 "lib/parsing/net_parser.mly"
                   ( Snd (e, gen_pos _startpos _endpos) )
# 1702 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1706 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_57 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 106 "lib/parsing/net_parser.mly"
                    ( Left (e, gen_pos _startpos _endpos) )
# 1716 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1720 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_58 =
  fun _endpos_e_ _startpos__1_ e ->
    let _endpos = _endpos_e_ in
    let _startpos = _startpos__1_ in
    (
# 107 "lib/parsing/net_parser.mly"
                     ( Right (e, gen_pos _startpos _endpos) )
# 1730 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1734 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_59 =
  fun _endpos_cases_ _startpos__1_ cases e ->
    let _endpos = _endpos_cases_ in
    let _startpos = _startpos__1_ in
    (
# 108 "lib/parsing/net_parser.mly"
                                                        ( Match (e, cases, gen_pos _startpos _endpos) )
# 1744 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1748 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_60 =
  fun _endpos__3_ _startpos__1_ e ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 109 "lib/parsing/net_parser.mly"
                             ( Net.set_info_expr (gen_pos _startpos _endpos) e )
# 1758 "lib/parsing/net_parser.ml"
     : (
# 57 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.expr)
# 1762 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_61 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 137 "lib/parsing/net_parser.mly"
           ( TUnit (gen_pos _startpos _endpos) )
# 1772 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1776 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_62 =
  fun _endpos_t_ _startpos__1_ t ->
    let _endpos = _endpos_t_ in
    let _startpos = _startpos__1_ in
    (
# 138 "lib/parsing/net_parser.mly"
                            ( TLoc (t, gen_pos _startpos _endpos) )
# 1786 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1790 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_63 =
  fun _endpos_t2_ _startpos_t1_ t1 t2 ->
    let _endpos = _endpos_t2_ in
    let _startpos = _startpos_t1_ in
    (
# 139 "lib/parsing/net_parser.mly"
                                  ( TMap (t1, t2, gen_pos _startpos _endpos) )
# 1800 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1804 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_64 =
  fun _endpos_t2_ _startpos_t1_ t1 t2 ->
    let _endpos = _endpos_t2_ in
    let _startpos = _startpos_t1_ in
    (
# 140 "lib/parsing/net_parser.mly"
                                  ( TProd (t1, t2, gen_pos _startpos _endpos) )
# 1814 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1818 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_65 =
  fun _endpos_t2_ _startpos_t1_ t1 t2 ->
    let _endpos = _endpos_t2_ in
    let _startpos = _startpos_t1_ in
    (
# 141 "lib/parsing/net_parser.mly"
                                 ( TSum (t1, t2, gen_pos _startpos _endpos) )
# 1828 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1832 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_66 =
  fun _endpos__3_ _startpos__1_ t ->
    let _endpos = _endpos__3_ in
    let _startpos = _startpos__1_ in
    (
# 142 "lib/parsing/net_parser.mly"
                             ( Net.set_info_typ (gen_pos _startpos _endpos) t )
# 1842 "lib/parsing/net_parser.ml"
     : (
# 58 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.typ)
# 1846 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_67 =
  fun e p ->
    let x = 
# 172 "lib/parsing/net_parser.mly"
                                           ( p, e )
# 1854 "lib/parsing/net_parser.ml"
     in
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1859 "lib/parsing/net_parser.ml"
     : (
# 70 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.pattern * Parsed_ast.Local.expr) list)
# 1863 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_68 =
  fun e p xs ->
    let x = 
# 172 "lib/parsing/net_parser.mly"
                                           ( p, e )
# 1871 "lib/parsing/net_parser.ml"
     in
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1876 "lib/parsing/net_parser.ml"
     : (
# 70 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.pattern * Parsed_ast.Local.expr) list)
# 1880 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_69 =
  fun x ->
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1888 "lib/parsing/net_parser.ml"
     : (
# 71 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern list)
# 1892 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_70 =
  fun x xs ->
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1900 "lib/parsing/net_parser.ml"
     : (
# 71 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.pattern list)
# 1904 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_71 =
  fun e p ->
    let x = 
# 175 "lib/parsing/net_parser.mly"
                                         ( p, e )
# 1912 "lib/parsing/net_parser.ml"
     in
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1917 "lib/parsing/net_parser.ml"
     : (
# 72 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.pattern * Parsed_ast.Net.expr) list)
# 1921 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_72 =
  fun e p xs ->
    let x = 
# 175 "lib/parsing/net_parser.mly"
                                         ( p, e )
# 1929 "lib/parsing/net_parser.ml"
     in
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1934 "lib/parsing/net_parser.ml"
     : (
# 72 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.pattern * Parsed_ast.Net.expr) list)
# 1938 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_73 =
  fun e sync ->
    let x = 
# 178 "lib/parsing/net_parser.mly"
                                         ( sync, e )
# 1946 "lib/parsing/net_parser.ml"
     in
    (
# 228 "<standard.mly>"
    ( [ x ] )
# 1951 "lib/parsing/net_parser.ml"
     : (
# 73 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.sync_label * Parsed_ast.Net.expr) list)
# 1955 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_74 =
  fun e sync xs ->
    let x = 
# 178 "lib/parsing/net_parser.mly"
                                         ( sync, e )
# 1963 "lib/parsing/net_parser.ml"
     in
    (
# 231 "<standard.mly>"
    ( x :: xs )
# 1968 "lib/parsing/net_parser.ml"
     : (
# 73 "lib/parsing/net_parser.mly"
      ((Parsed_ast.Local.sync_label * Parsed_ast.Net.expr) list)
# 1972 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_75 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 1980 "lib/parsing/net_parser.ml"
     : (
# 74 "lib/parsing/net_parser.mly"
      (unit option)
# 1984 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_76 =
  fun x ->
    (
# 114 "<standard.mly>"
    ( Some x )
# 1992 "lib/parsing/net_parser.ml"
     : (
# 74 "lib/parsing/net_parser.mly"
      (unit option)
# 1996 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_77 =
  fun _1 ->
    (
# 81 "lib/parsing/net_parser.mly"
                   ( _1 )
# 2004 "lib/parsing/net_parser.ml"
     : (
# 54 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt_block)
# 2008 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_78 =
  fun _endpos__4_ _startpos_p_ p t ->
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_p_ in
    (
# 87 "lib/parsing/net_parser.mly"
                                               ( Decl (p, t, gen_pos _startpos _endpos) )
# 2018 "lib/parsing/net_parser.ml"
     : (
# 56 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 2022 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_79 =
  fun _endpos__4_ _startpos_ps_ e ps ->
    let _endpos = _endpos__4_ in
    let _startpos = _startpos_ps_ in
    (
# 88 "lib/parsing/net_parser.mly"
                                                                 ( Assign (ps, e, gen_pos _startpos _endpos) )
# 2032 "lib/parsing/net_parser.ml"
     : (
# 56 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 2036 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_80 =
  fun _endpos__5_ _startpos__1_ id t ->
    let _endpos = _endpos__5_ in
    let _startpos = _startpos__1_ in
    (
# 89 "lib/parsing/net_parser.mly"
                                                 ( TypeDecl (id, t, gen_pos _startpos _endpos) )
# 2046 "lib/parsing/net_parser.ml"
     : (
# 56 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 2050 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_81 =
  fun f ->
    (
# 90 "lib/parsing/net_parser.mly"
                   ( f )
# 2058 "lib/parsing/net_parser.ml"
     : (
# 56 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt)
# 2062 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_82 =
  fun _1 ->
    (
# 84 "lib/parsing/net_parser.mly"
               ( _1 )
# 2070 "lib/parsing/net_parser.ml"
     : (
# 55 "lib/parsing/net_parser.mly"
      (Parsed_ast.Net.stmt_block)
# 2074 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_83 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 163 "lib/parsing/net_parser.mly"
          ( LabelId (id, gen_pos _startpos _endpos) )
# 2084 "lib/parsing/net_parser.ml"
     : (
# 67 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.sync_label)
# 2088 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_84 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 160 "lib/parsing/net_parser.mly"
          ( TypId (id, gen_pos _startpos _endpos) )
# 2098 "lib/parsing/net_parser.ml"
     : (
# 66 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.typ_id)
# 2102 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_85 =
  fun _endpos_i_ _startpos_i_ i ->
    let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    (
# 166 "lib/parsing/net_parser.mly"
          ( Int (i, gen_pos _startpos _endpos) )
# 2112 "lib/parsing/net_parser.ml"
     : (
# 63 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.value)
# 2116 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_86 =
  fun _endpos_s_ _startpos_s_ s ->
    let _endpos = _endpos_s_ in
    let _startpos = _startpos_s_ in
    (
# 167 "lib/parsing/net_parser.mly"
             ( String (s, gen_pos _startpos _endpos) )
# 2126 "lib/parsing/net_parser.ml"
     : (
# 63 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.value)
# 2130 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_87 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 168 "lib/parsing/net_parser.mly"
         ( Bool (true, gen_pos _startpos _endpos) )
# 2140 "lib/parsing/net_parser.ml"
     : (
# 63 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.value)
# 2144 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_88 =
  fun _endpos__1_ _startpos__1_ ->
    let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    (
# 169 "lib/parsing/net_parser.mly"
          ( Bool (false, gen_pos _startpos _endpos) )
# 2154 "lib/parsing/net_parser.ml"
     : (
# 63 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.value)
# 2158 "lib/parsing/net_parser.ml"
    ))

let _menhir_action_89 =
  fun _endpos_id_ _startpos_id_ id ->
    let _endpos = _endpos_id_ in
    let _startpos = _startpos_id_ in
    (
# 157 "lib/parsing/net_parser.mly"
          ( VarId (id, gen_pos _startpos _endpos) )
# 2168 "lib/parsing/net_parser.ml"
     : (
# 65 "lib/parsing/net_parser.mly"
      (Parsed_ast.Local.var_id)
# 2172 "lib/parsing/net_parser.ml"
    ))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ALLOW ->
        "ALLOW"
    | AND ->
        "AND"
    | ARROW ->
        "ARROW"
    | BAR ->
        "BAR"
    | BOOL_T ->
        "BOOL_T"
    | CHOICE ->
        "CHOICE"
    | CHOOSE ->
        "CHOOSE"
    | COLON ->
        "COLON"
    | COLONEQ ->
        "COLONEQ"
    | COMMA ->
        "COMMA"
    | DIV ->
        "DIV"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FOR ->
        "FOR"
    | FOREIGN ->
        "FOREIGN"
    | FROM ->
        "FROM"
    | FST ->
        "FST"
    | FUN ->
        "FUN"
    | GEQ ->
        "GEQ"
    | GT ->
        "GT"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT _ ->
        "INT"
    | INT_T ->
        "INT_T"
    | LEFT ->
        "LEFT"
    | LEQ ->
        "LEQ"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MATCH ->
        "MATCH"
    | MINUS ->
        "MINUS"
    | NEQ ->
        "NEQ"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | RECV ->
        "RECV"
    | RET ->
        "RET"
    | RIGHT ->
        "RIGHT"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | SEND ->
        "SEND"
    | SND ->
        "SND"
    | STRING _ ->
        "STRING"
    | STRING_T ->
        "STRING_T"
    | THEN ->
        "THEN"
    | TILDE_ARROW ->
        "TILDE_ARROW"
    | TIMES ->
        "TIMES"
    | TRUE ->
        "TRUE"
    | TYPE ->
        "TYPE"
    | UNDERSCORE ->
        "UNDERSCORE"
    | UNIT_T ->
        "UNIT_T"
    | WITH ->
        "WITH"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_058 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_77 _1 in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_30 _endpos__1_ _startpos__1_ in
      _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_local_pattern : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState188 ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState175 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState037 ->
          _menhir_run_045 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_189 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState190 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_44 _endpos__1_ _startpos__1_ in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_net_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState062 ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState064 ->
          _menhir_run_198 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState065 ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState190 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState183 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState135 ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState139 ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState171 ->
          _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState169 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState144 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState150 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState158 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_199 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_nonempty_list_local_pattern_ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMICOLON ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_nonempty_list_local_pattern_ (_menhir_stack, _menhir_s, ps, _startpos_ps_) = _menhir_stack in
          let (e, _endpos__4_) = (_v, _endpos_0) in
          let _v = _menhir_action_79 _endpos__4_ _startpos_ps_ e ps in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | TYPE ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | STRING _v_0 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState060
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | INT _v_1 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState060
      | ID _v_2 ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState060
      | FOREIGN ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | EOF | IN ->
          let _v_3 = _menhir_action_02 () in
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          let _startpos_0 = _menhir_lexbuf.Lexing.lex_start_p in
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos_0, _v) in
          let _v = _menhir_action_84 _endpos_id_ _startpos_id_ id in
          let _menhir_stack = MenhirCell0_typ_id (_menhir_stack, _v) in
          (match (_tok : MenhirBasics.token) with
          | COLONEQ ->
              let _menhir_s = MenhirState005 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | UNIT_T ->
                  _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ID _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_006 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_61 _endpos__1_ _startpos__1_ in
      _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_net_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState176 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState007 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_177 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_COLON as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COLON (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_local_pattern (_menhir_stack, _menhir_s, p, _startpos_p_, _) = _menhir_stack in
          let (t, _endpos__4_) = (_v, _endpos_0) in
          let _v = _menhir_action_78 _endpos__4_ _startpos_p_ p t in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_010 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_net_type -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_TIMES (_menhir_stack, _startpos, _endpos) in
      let _menhir_s = MenhirState010 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState007 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_04 _endpos_id_ _startpos_id_ id in
      _menhir_goto_loc_id _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_goto_loc_id : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState196 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState153 ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState148 ->
          _menhir_run_149 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState176 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState005 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState010 ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_197 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SEND, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_net_expr (_menhir_stack, _, e, _) = _menhir_stack in
      let MenhirCell1_SEND (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_id_, id) = (_endpos, _v) in
      let _v = _menhir_action_49 _endpos_id_ _startpos__1_ e id in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _v _menhir_s _tok
  
  and _menhir_run_154 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ALLOW as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_loc_id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_s = MenhirState155 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BAR ->
              _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_156 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState156 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_146 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_83 _endpos_id_ _startpos_id_ id in
      _menhir_goto_sync_label _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_sync_label : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState156 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_147 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_157 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_sync_label (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState158 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SND (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState064 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SEND (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState065 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_066 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState066 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RET (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState067 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_87 _endpos__1_ _startpos__1_ in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState067 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState175 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_083 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_06 _endpos_v_ _startpos_v_ v in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_v_ _v _menhir_s _tok
  
  and _menhir_goto_local_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState067 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState071 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_118 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_106 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState087 ->
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState081 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_130 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_RET as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState130
      | BAR | COMMA | ELSE | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_RET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_46 _endpos_e_ _startpos__1_ e in
          _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_085 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_TIMES (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState085 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_s_, _startpos_s_, s) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_86 _endpos_s_ _startpos_s_ s in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_s_ _startpos_s_ _v _menhir_s _tok
  
  and _menhir_run_068 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_SND (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState068 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_069 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState069 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState070 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState071 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_072 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState072 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | STRING _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073
      | SND ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | RPAREN ->
          let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_startpos__1_, _endpos__2_) = (_startpos, _endpos) in
          let _v = _menhir_action_05 _endpos__2_ _startpos__1_ in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__2_ _startpos__1_ _v _menhir_s _tok
      | RIGHT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | NOT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | MINUS ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | MATCH ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | LET ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | LEFT ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | INT _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073
      | ID _v ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073
      | FST ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | FALSE ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState075 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_040 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_89 _endpos_id_ _startpos_id_ id in
      _menhir_goto_var_id _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_goto_var_id : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState062 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState064 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState066 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState134 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState183 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState135 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState171 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState169 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState140 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState144 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState150 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState067 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState069 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState070 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState072 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState115 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState111 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState079 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState105 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState099 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState095 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState093 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState089 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState087 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState188 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState175 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState166 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState038 ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_159 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_45 _endpos_id_ _startpos_id_ id in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _v _menhir_s _tok
  
  and _menhir_run_082 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_id_, _startpos_id_, id) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_07 _endpos_id_ _startpos_id_ id in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _startpos_id_ _v _menhir_s _tok
  
  and _menhir_run_076 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState077 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_T ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_T ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_T ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_37 _endpos__1_ _startpos__1_ in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_goto_local_type : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState013 ->
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState023 ->
          _menhir_run_024 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState016 ->
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_078 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLONEQ ->
          let _menhir_s = MenhirState079 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SND ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FST ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_local_type -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_TIMES (_menhir_stack, _startpos, _endpos) in
      let _menhir_s = MenhirState020 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_T ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_T ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_T ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_39 _endpos__1_ _startpos__1_ in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState016 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_T ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_T ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_T ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_38 _endpos__1_ _startpos__1_ in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_40 _endpos__1_ _startpos__1_ in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_local_type -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_PLUS (_menhir_stack, _startpos, _endpos) in
      let _menhir_s = MenhirState023 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING_T ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT_T ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOOL_T ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_080 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState080 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos_i_, _startpos_i_, i) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_85 _endpos_i_ _startpos_i_ i in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_i_ _startpos_i_ _v _menhir_s _tok
  
  and _menhir_run_081 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FST (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState081 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let (_endpos__1_, _startpos__1_) = (_endpos, _startpos) in
      let _v = _menhir_action_88 _endpos__1_ _startpos__1_ in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__1_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_025 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_loc_id -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_loc_id (_menhir_stack, _menhir_s, _, _startpos__1_, _) = _menhir_stack in
      let (_endpos_t_, t) = (_endpos, _v) in
      let _v = _menhir_action_62 _endpos_t_ _startpos__1_ t in
      _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_024 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_type _menhir_cell0_PLUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW | COLONEQ | EOF | FALSE | FOREIGN | ID _ | IN | INT _ | LEFT | LPAREN | PLUS | RIGHT | RPAREN | SEMICOLON | STRING _ | TRUE | TYPE | UNDERSCORE ->
          let MenhirCell0_PLUS (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_local_type (_menhir_stack, _menhir_s, t1, _startpos_t1_, _) = _menhir_stack in
          let (_endpos_t2_, t2) = (_endpos, _v) in
          let _v = _menhir_action_42 _endpos_t2_ _startpos_t1_ t1 t2 in
          _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t2_ _startpos_t1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_local_type _menhir_cell0_TIMES -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_TIMES (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_local_type (_menhir_stack, _menhir_s, t1, _startpos_t1_, _) = _menhir_stack in
      let (_endpos_t2_, t2) = (_endpos, _v) in
      let _v = _menhir_action_41 _endpos_t2_ _startpos_t1_ t1 t2 in
      _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t2_ _startpos_t1_ _v _menhir_s _tok
  
  and _menhir_run_019 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, t) = (_endpos_0, _v) in
          let _v = _menhir_action_43 _endpos__3_ _startpos__1_ t in
          _menhir_goto_local_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FOREIGN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_var_id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _menhir_s = MenhirState053 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_x_, _startpos_x_, x) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_32 _endpos_x_ _startpos_x_ x in
      _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_087 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState087 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_091 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_OR (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_093 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_NEQ (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState093 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_095 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState095 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_LT (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_LEQ (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState099 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_GT (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState101 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_GEQ (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState103 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_105 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_EQ (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState105 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_089 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_DIV (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState089 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr as 'stack) -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell1_AND (_menhir_stack, _menhir_s, _startpos, _endpos) in
      let _menhir_s = MenhirState107 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SND ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FST ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_129 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SND as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_SND (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_25 _endpos_e_ _startpos__1_ e in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_128 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_RIGHT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState128
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_27 _endpos_e_ _startpos__1_ e in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_127 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_09 _endpos__1_ _endpos_e_ _startpos__1_ e in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_126 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_08 _endpos__1_ _endpos_e_ _startpos__1_ e in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_123 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | BAR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
      | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_local_pattern (_menhir_stack, _, p, _, _) = _menhir_stack in
          let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_67 e p in
          _menhir_goto_nonempty_list_local_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_120 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState120 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState036 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState038 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_local_case_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState119 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState123 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_125 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_WITH -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_WITH (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_local_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_cases_, cases) = (_endpos, _v) in
      let _v = _menhir_action_28 _endpos_cases_ _startpos__1_ cases e in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_cases_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_124 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_local_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_local_expr (_menhir_stack, _, e, _, _) = _menhir_stack in
      let MenhirCell1_local_pattern (_menhir_stack, _, p, _, _) = _menhir_stack in
      let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_68 e p xs in
      _menhir_goto_nonempty_list_local_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_118 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_stack = MenhirCell1_WITH (_menhir_stack, MenhirState118) in
          let _menhir_s = MenhirState119 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BAR ->
              _menhir_run_120 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | TIMES ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | PLUS ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | OR ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | NEQ ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | MINUS ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | LT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | LEQ ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | GT ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | GEQ ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | EQ ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | DIV ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | AND ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState118
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_COMMA as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, e2) = (_endpos_0, _v) in
          let _v = _menhir_action_23 _endpos__5_ _startpos__1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | _ ->
          _eRR ()
  
  and _menhir_run_113 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos_0, _v) in
          let _v = _menhir_action_29 _endpos__3_ _startpos__1_ e in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | COMMA ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, MenhirState113) in
          let _menhir_s = MenhirState115 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SND ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FST ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
      | _ ->
          _eRR ()
  
  and _menhir_run_112 : type  ttv_stack. ((((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_IN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState112
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_IN (_menhir_stack, _) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _, e1, _, _) = _menhir_stack in
          let MenhirCell1_local_type (_menhir_stack, _, t, _, _) = _menhir_stack in
          let MenhirCell1_var_id (_menhir_stack, _, id, _, _) = _menhir_stack in
          let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_22 _endpos_e2_ _startpos__1_ e1 e2 id t in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_110 : type  ttv_stack. ((((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_var_id, _menhir_box_prog) _menhir_cell1_local_type as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | PLUS ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | OR ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | NEQ ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | MINUS ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LT ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | LEQ ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | IN ->
          let _menhir_stack = MenhirCell1_IN (_menhir_stack, MenhirState110) in
          let _menhir_s = MenhirState111 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SND ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FST ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | GT ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | GEQ ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | EQ ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | DIV ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | AND ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState110
      | _ ->
          _eRR ()
  
  and _menhir_run_109 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LEFT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_26 _endpos_e_ _startpos__1_ e in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_AND as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState108
      | BAR | COMMA | ELSE | IN | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_AND (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_14 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_106 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_EQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_EQ (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_16 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_104 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GEQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_GEQ (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_21 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_GT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_GT (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_20 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_100 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LEQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_LEQ (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_19 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_LT as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_LT (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_18 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_MINUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_MINUS (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_11 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_094 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_NEQ as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | NEQ | OR | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_NEQ (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_17 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_OR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_OR (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_15 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_DIV -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_DIV (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
      let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_13 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_088 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_PLUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
      | AND | BAR | COMMA | ELSE | EQ | GEQ | GT | IN | LEQ | LT | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_PLUS (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
          let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
          let (_endpos_e2_, e2) = (_endpos, _v) in
          let _v = _menhir_action_10 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_086 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_local_expr, _menhir_box_prog) _menhir_cell1_TIMES -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_TIMES (_menhir_stack, _, _startpos__1_, _endpos__1_) = _menhir_stack in
      let MenhirCell1_local_expr (_menhir_stack, _menhir_s, e1, _startpos_e1_, _) = _menhir_stack in
      let (_endpos_e2_, e2) = (_endpos, _v) in
      let _v = _menhir_action_12 _endpos__1_ _endpos_e2_ _startpos__1_ _startpos_e1_ e1 e2 in
      _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e2_ _startpos_e1_ _v _menhir_s _tok
  
  and _menhir_run_084 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FST as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | PLUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | OR ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | NEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | MINUS ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | LT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | LEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | GT ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | GEQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | EQ ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | DIV ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | AND ->
          let _menhir_stack = MenhirCell1_local_expr (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState084
      | BAR | COMMA | ELSE | IN | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_FST (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_24 _endpos_e_ _startpos__1_ e in
          _menhir_goto_local_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let (_endpos_v_, _startpos_v_, v) = (_endpos, _startpos, _v) in
      let _v = _menhir_action_31 _endpos_v_ _startpos_v_ v in
      _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_v_ _startpos_v_ _v _menhir_s _tok
  
  and _menhir_run_131 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_RECV (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | FROM ->
          let _menhir_s = MenhirState132 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_134 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState134 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_135 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState135 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_136 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | TYPE ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136
      | FOREIGN ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
      | IN ->
          let _v = _menhir_action_02 () in
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FOREIGN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState051 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_179 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_82 _1 in
      _menhir_goto_stmt_block _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt_block : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_058 _menhir_stack _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_137 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt_block (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState138 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_139 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState139 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_140 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState140 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_141 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState141 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_144 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_FST (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState144 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SND ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SEND ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RIGHT ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RET ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | RECV ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MATCH ->
          _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LET ->
          _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FUN ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHOOSE ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ALLOW ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_145 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_CHOOSE (_menhir_stack, _menhir_s, _startpos) in
      let _menhir_s = MenhirState145 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ID _v ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _menhir_stack = MenhirCell1_ALLOW (_menhir_stack, _menhir_s, _startpos) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | CHOICE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | FROM ->
              let _menhir_s = MenhirState153 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | ID _v ->
                  _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_147 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_CHOOSE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_sync_label (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | FOR ->
          let _menhir_s = MenhirState148 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_149 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_CHOOSE, _menhir_box_prog) _menhir_cell1_sync_label as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_loc_id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | IN ->
          let _menhir_s = MenhirState150 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_RECV -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_RECV (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_id_, id) = (_endpos, _v) in
      let _v = _menhir_action_50 _endpos_id_ _startpos__1_ id in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_id_ _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_loc_id (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | DOT ->
          let _menhir_s = MenhirState013 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING_T ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT_T ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | BOOL_T ->
              _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_027 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_net_type -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let _endpos = _menhir_lexbuf.Lexing.lex_curr_p in
      let _menhir_stack = MenhirCell0_PLUS (_menhir_stack, _startpos, _endpos) in
      let _menhir_s = MenhirState027 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_net_type -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNIT_T ->
          _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_054 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_FOREIGN, _menhir_box_prog) _menhir_cell1_var_id as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLONEQ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STRING _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | SEMICOLON ->
                  let _endpos_3 = _menhir_lexbuf.Lexing.lex_curr_p in
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let MenhirCell1_var_id (_menhir_stack, _, id, _, _) = _menhir_stack in
                  let MenhirCell1_FOREIGN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
                  let (s, _endpos__7_, t) = (_v_0, _endpos_3, _v) in
                  let _v = _menhir_action_01 _endpos__7_ _startpos__1_ id s t in
                  let f = _v in
                  let _v = _menhir_action_81 f in
                  _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | ARROW ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TYPE _menhir_cell0_typ_id as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | SEMICOLON ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let (_endpos_x_, x) = (_endpos_0, ()) in
          let _ = _menhir_action_76 x in
          _menhir_goto_option_SEMICOLON_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_x_ _tok
      | PLUS ->
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | FALSE | FOREIGN | ID _ | IN | INT _ | LEFT | LPAREN | RIGHT | STRING _ | TRUE | TYPE | UNDERSCORE ->
          let _ = _menhir_action_75 () in
          _menhir_goto_option_SEMICOLON_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_SEMICOLON_ : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_TYPE _menhir_cell0_typ_id, _menhir_box_prog) _menhir_cell1_net_type -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _tok ->
      let MenhirCell1_net_type (_menhir_stack, _, t, _, _) = _menhir_stack in
      let MenhirCell0_typ_id (_menhir_stack, id) = _menhir_stack in
      let MenhirCell1_TYPE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let _endpos__5_ = _endpos in
      let _v = _menhir_action_80 _endpos__5_ _startpos__1_ id t in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_030 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_net_type as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLONEQ | EOF | FALSE | FOREIGN | ID _ | IN | INT _ | LEFT | LPAREN | RIGHT | RPAREN | SEMICOLON | STRING _ | TRUE | TYPE | UNDERSCORE ->
          let MenhirCell1_net_type (_menhir_stack, _menhir_s, t1, _startpos_t1_, _) = _menhir_stack in
          let (_endpos_t2_, t2) = (_endpos, _v) in
          let _v = _menhir_action_63 _endpos_t2_ _startpos_t1_ t1 t2 in
          _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t2_ _startpos_t1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_net_type _menhir_cell0_PLUS as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW | COLONEQ | EOF | FALSE | FOREIGN | ID _ | IN | INT _ | LEFT | LPAREN | PLUS | RIGHT | RPAREN | SEMICOLON | STRING _ | TRUE | TYPE | UNDERSCORE ->
          let MenhirCell0_PLUS (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_net_type (_menhir_stack, _menhir_s, t1, _startpos_t1_, _) = _menhir_stack in
          let (_endpos_t2_, t2) = (_endpos, _v) in
          let _v = _menhir_action_65 _endpos_t2_ _startpos_t1_ t1 t2 in
          _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t2_ _startpos_t1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_011 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_net_type _menhir_cell0_TIMES -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell0_TIMES (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_net_type (_menhir_stack, _menhir_s, t1, _startpos_t1_, _) = _menhir_stack in
      let (_endpos_t2_, t2) = (_endpos, _v) in
      let _v = _menhir_action_64 _endpos_t2_ _startpos_t1_ t1 t2 in
      _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_t2_ _startpos_t1_ _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, t) = (_endpos_0, _v) in
          let _v = _menhir_action_66 _endpos__3_ _startpos__1_ t in
          _menhir_goto_net_type _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | PLUS ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_027 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ARROW ->
          let _menhir_stack = MenhirCell1_net_type (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_201 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_03 x xs in
      _menhir_goto_list_stmt_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_stmt_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState060 ->
          _menhir_run_201 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState000 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_198 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_SND -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_SND (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_56 _endpos_e_ _startpos__1_ e in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_195 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_SEND as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | TILDE_ARROW ->
          let _menhir_s = MenhirState196 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ID _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_194 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_RIGHT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_58 _endpos_e_ _startpos__1_ e in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_191 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState191
      | COMMA | ELSE | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_local_pattern (_menhir_stack, _, p, _, _) = _menhir_stack in
          let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_71 e p in
          _menhir_goto_nonempty_list_net_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_188 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState188 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_net_case_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState187 ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState191 ->
          _menhir_run_192 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_193 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_MATCH, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_net_expr (_menhir_stack, _, e, _) = _menhir_stack in
      let MenhirCell1_MATCH (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_cases_, cases) = (_endpos, _v) in
      let _v = _menhir_action_59 _endpos_cases_ _startpos__1_ cases e in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_cases_ _v _menhir_s _tok
  
  and _menhir_run_192 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_local_pattern, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_net_expr (_menhir_stack, _, e, _) = _menhir_stack in
      let MenhirCell1_local_pattern (_menhir_stack, _, p, _, _) = _menhir_stack in
      let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_72 e p xs in
      _menhir_goto_nonempty_list_net_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_186 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_MATCH as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | WITH ->
          let _menhir_s = MenhirState187 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | BAR ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_184 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_net_expr (_menhir_stack, _, e1, _) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, e2) = (_endpos_0, _v) in
          let _v = _menhir_action_54 _endpos__5_ _startpos__1_ e1 e2 in
          _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_181 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__3_, e) = (_endpos_0, _v) in
          let _v = _menhir_action_60 _endpos__3_ _startpos__1_ e in
          _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
          let _menhir_s = MenhirState183 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_174 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LET, _menhir_box_prog) _menhir_cell1_stmt_block -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_stmt_block (_menhir_stack, _, stmts) = _menhir_stack in
      let MenhirCell1_LET (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_48 _endpos_e_ _startpos__1_ e stmts in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_173 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LEFT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_57 _endpos_e_ _startpos__1_ e in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_172 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_net_expr, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_net_expr (_menhir_stack, _, e2, _) = _menhir_stack in
      let MenhirCell1_net_expr (_menhir_stack, _, e1, _) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e3_, e3) = (_endpos, _v) in
      let _v = _menhir_action_47 _endpos_e3_ _startpos__1_ e1 e2 e3 in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e3_ _v _menhir_s _tok
  
  and _menhir_run_170 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_IF, _menhir_box_prog) _menhir_cell1_net_expr as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState171 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_168 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState169 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_165 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FUN, _menhir_box_prog) _menhir_cell1_nonempty_list_local_pattern_ -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_nonempty_list_local_pattern_ (_menhir_stack, _, ps, _) = _menhir_stack in
      let MenhirCell1_FUN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_53 _endpos_e_ _startpos__1_ e ps in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_164 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_FST -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_FST (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_55 _endpos_e_ _startpos__1_ e in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_163 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_CHOOSE, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_cell1_loc_id -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_loc_id (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_sync_label (_menhir_stack, _, sync) = _menhir_stack in
      let MenhirCell1_CHOOSE (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_e_, e) = (_endpos, _v) in
      let _v = _menhir_action_51 _endpos_e_ _startpos__1_ e id sync in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
  
  and _menhir_run_160 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_sync_label as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | BAR ->
          let _menhir_stack = MenhirCell1_net_expr (_menhir_stack, _menhir_s, _v, _endpos) in
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState160
      | COMMA | ELSE | RPAREN | SEMICOLON | THEN | TILDE_ARROW | WITH ->
          let MenhirCell1_sync_label (_menhir_stack, _, sync) = _menhir_stack in
          let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let (_endpos_e_, e) = (_endpos, _v) in
          let _v = _menhir_action_73 e sync in
          _menhir_goto_nonempty_list_sync_choice_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_e_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_sync_choice_case_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState155 ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | MenhirState160 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_162 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_ALLOW, _menhir_box_prog) _menhir_cell1_loc_id -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_loc_id (_menhir_stack, _, id, _, _) = _menhir_stack in
      let MenhirCell1_ALLOW (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_cases_, cases) = (_endpos, _v) in
      let _v = _menhir_action_52 _endpos_cases_ _startpos__1_ cases id in
      _menhir_goto_net_expr _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_cases_ _v _menhir_s _tok
  
  and _menhir_run_161 : type  ttv_stack. (((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR, _menhir_box_prog) _menhir_cell1_sync_label, _menhir_box_prog) _menhir_cell1_net_expr -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_net_expr (_menhir_stack, _, e, _) = _menhir_stack in
      let MenhirCell1_sync_label (_menhir_stack, _, sync) = _menhir_stack in
      let MenhirCell1_BAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let (_endpos_xs_, xs) = (_endpos, _v) in
      let _v = _menhir_action_74 e sync xs in
      _menhir_goto_nonempty_list_sync_choice_case_ _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_xs_ _v _menhir_s _tok
  
  and _menhir_run_175 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | TRUE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState175
      | RIGHT ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | LPAREN ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | LEFT ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState175
      | ID _v_2 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState175
      | FALSE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState175
      | COLON ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_stack = MenhirCell1_COLON (_menhir_stack, MenhirState175) in
          let _menhir_s = MenhirState176 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_006 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | COLONEQ ->
          let (_startpos_x_, x) = (_startpos, _v) in
          let _v = _menhir_action_69 x in
          _menhir_goto_nonempty_list_local_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_nonempty_list_local_pattern_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState175 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState166 ->
          _menhir_run_167 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_167 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_local_pattern -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_local_pattern (_menhir_stack, _menhir_s, x, _startpos_x_, _) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_70 x xs in
      _menhir_goto_nonempty_list_local_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
  
  and _menhir_run_142 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_FUN as 'stack) -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nonempty_list_local_pattern_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState143 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_061 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nonempty_list_local_pattern_ (_menhir_stack, _menhir_s, _v, _startpos) in
      match (_tok : MenhirBasics.token) with
      | COLONEQ ->
          let _menhir_s = MenhirState062 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNIT_T ->
              _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SND ->
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SEND ->
              _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RET ->
              _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RECV ->
              _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_134 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_136 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FUN ->
              _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHOOSE ->
              _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ALLOW ->
              _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_166 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | TRUE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | STRING _v_0 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState166
      | RIGHT ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | LPAREN ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | LEFT ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | INT _v_1 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState166
      | ID _v_2 ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState166
      | FALSE ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState166
      | ARROW | COLONEQ ->
          let (_startpos_x_, x) = (_startpos, _v) in
          let _v = _menhir_action_69 x in
          _menhir_goto_nonempty_list_local_pattern_ _menhir_stack _menhir_lexbuf _menhir_lexer _startpos_x_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_121 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_BAR as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
      match (_tok : MenhirBasics.token) with
      | ARROW ->
          let _menhir_s = MenhirState122 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRUE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SND ->
              _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RIGHT ->
              _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MATCH ->
              _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LET ->
              _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FST ->
              _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FALSE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_050 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_RIGHT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_RIGHT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_p_, p) = (_endpos, _v) in
      let _v = _menhir_action_35 _endpos_p_ _startpos__1_ p in
      _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_p_ _startpos__1_ _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN, _menhir_box_prog) _menhir_cell1_local_pattern -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_local_pattern (_menhir_stack, _, p1, _, _) = _menhir_stack in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (_endpos__5_, p2) = (_endpos_0, _v) in
          let _v = _menhir_action_33 _endpos__5_ _startpos__1_ p1 p2 in
          _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__5_ _startpos__1_ _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_045 : type  ttv_stack. ((ttv_stack, _menhir_box_prog) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> _ -> _ -> ('stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _startpos _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _endpos_0 = _menhir_lexbuf.Lexing.lex_curr_p in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
          let (p, _endpos__3_) = (_v, _endpos_0) in
          let _v = _menhir_action_36 _endpos__3_ _startpos__1_ p in
          _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos__3_ _startpos__1_ _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_local_pattern (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
          let _menhir_s = MenhirState047 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | UNDERSCORE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TRUE ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRING _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | RIGHT ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LEFT ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FALSE ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_044 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_LEFT -> _ -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _endpos _v _tok ->
      let MenhirCell1_LEFT (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
      let (_endpos_p_, p) = (_endpos, _v) in
      let _v = _menhir_action_34 _endpos_p_ _startpos__1_ p in
      _menhir_goto_local_pattern _menhir_stack _menhir_lexbuf _menhir_lexer _endpos_p_ _startpos__1_ _v _menhir_s _tok
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | UNDERSCORE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TYPE ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TRUE ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | STRING _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | RIGHT ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | LEFT ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | INT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | ID _v ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000
      | FOREIGN ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | FALSE ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_02 () in
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
