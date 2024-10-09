val emit_toplevel_shm : Format.formatter ->
    (module Msg_intf.M) ->
    string list ->
    Ast.Net.stmt_block list ->
    unit
