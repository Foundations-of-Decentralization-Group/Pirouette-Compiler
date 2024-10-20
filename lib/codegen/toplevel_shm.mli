val emit_toplevel_shm : out_channel ->
    (module Msg_intf.M) ->
    string list ->
    Ast.Net.stmt_block list ->
    unit