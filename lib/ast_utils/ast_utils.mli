(** AST utility functions.

    Provides shared helper functions for working with AST structures across
    the compiler.  Includes routines for:

    - Traversal and extraction – e.g., [extract_locs] collects all
      choreography locations from a statement block.
    - Serialization and pretty-printing– functions prefixed with
      [jsonify_*] and [pprint_*] convert ASTs to JSON or formatted source text.
    - Graph visualization – [stringify_dot_choreo_ast] and [dot_choreo_ast]
      export a choreography AST to DOT format for visual inspection.
    - Foreign function interface (FFI) utilities

    These utilities are intended for developer and tooling use (testing,
    visualization, and compiler diagnostics), not for end-user programs.
*)

val extract_locs : 'a Ast_core.Choreo.M.stmt_block -> string list
val stringify_jsonify_choreo_ast : 'a Ast_core.Choreo.M.stmt_block -> string
val stringify_jsonify_net_ast : 'a Ast_core.Net.M.stmt_block -> string
val jsonify_choreo_ast : out_channel -> 'a Ast_core.Choreo.M.stmt_block -> unit
val jsonify_net_ast : out_channel -> 'a Ast_core.Net.M.stmt_block -> unit
val stringify_pprint_choreo_ast : 'a Ast_core.Choreo.M.stmt_block -> string
val stringify_pprint_net_ast : 'a Ast_core.Net.M.stmt_block -> string
val pprint_choreo_ast : out_channel -> 'a Ast_core.Choreo.M.stmt_block -> unit
val pprint_net_ast : out_channel -> 'a Ast_core.Net.M.stmt_block -> unit
val stringify_dot_choreo_ast : ('a -> string) -> 'a Ast_core.Choreo.M.stmt_block -> string

val dot_choreo_ast
  :  out_channel
  -> ('a -> string)
  -> 'a Ast_core.Choreo.M.stmt_block
  -> unit

(* FFI extraction utilities *)
val parse_external_name : string -> (string option * string * string option)
val collect_ffi_info : 'a Ast_core.Choreo.M.stmt list -> (string option * string * string option) list
