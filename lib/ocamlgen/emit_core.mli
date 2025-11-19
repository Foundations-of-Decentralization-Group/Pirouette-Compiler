(** OCaml code generation from network IR.
    
    This module converts network IR (after endpoint projection) into executable
    OCaml code using PPX metaprogramming. It generates OCaml Abstract Syntax 
    Tree (AST) nodes via Ppxlib that can be compiled into runnable programs.
    
    The emission process:
    1. Network IR expressions/patterns → OCaml Parsetree expressions/patterns
    2. Communication primitives (Send/Recv) → Generated via pluggable message interface
    3. Local computations → Direct OCaml expressions
    
    The [Msg_intf.M] module provides pluggable code generation for 
    communication operations, allowing different runtime implementations
    to generate appropriate OCaml code for their backend.*)

(** {1 Exceptions}*)

exception Main_expr of Ppxlib.expression
(** [Main_expr] is raised when encountering the main expression during
      code generation.
      
      This exception carries the OCaml expression that represents the program's
      entry point, allowing special handling of the main computation separately
      from other bindings. *)

(** {1 Local Code Emission}
    
    Functions for converting local AST nodes (pure computation without 
    communication) to OCaml code. These don't require a message module
    since they generate pure functional code. *)

val emit_local_pexp : 'a Ast_core.Local.M.expr -> Ppxlib.expression
val emit_local_ppat : 'a Ast_core.Local.M.pattern -> Ppxlib.pattern

val emit_net_fun_body
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Local.M.pattern list
  -> 'a Ast_core.Net.M.expr
  -> Ppxlib.expression

val emit_net_binding
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Net.M.stmt
  -> Ppxlib.value_binding

(** {1 Network Code Emission}
    
    Functions for converting network IR nodes (with explicit communication)
    to OCaml code using a pluggable message interface.
    
    All network emission functions require:
    - [self_id]: The name of the current endpoint (e.g., "Alice", "Bob")
    - [msg_module]: Code generator module implementing [Msg_intf.M] that 
      produces OCaml expressions for send/recv operations *)

val emit_net_pexp
  :  self_id:string
  -> (module Msg_intf.M)
  -> 'a Ast_core.Net.M.expr
  -> Ppxlib.expression

val emit_foreign_decl : string -> 'a Ast_core.Net.M.typ -> string -> Ppxlib.value_binding
