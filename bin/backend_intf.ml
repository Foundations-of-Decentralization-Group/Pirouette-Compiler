open Controlang.Ctrl
(* open Basictypes *)

type med =
  | InterThread
  | Distributed
[@@deriving show]

type value = 
  | IntVal of int
  | StrVal of string
  | BoolVal of bool
[@@deriving show]

type astType = Ast of { code : ctrl; prop : string }

module type Comm = sig
  val init : string list -> string
  val send : string -> string -> string -> string
  val rcv : string -> string -> string -> string -> string
  val exit : string list -> string
end

module type Backend = sig
  val ext : string
  val codify : ctrl -> string list -> string -> string
  val format_and_save_code : string -> string -> unit
  val main : astType list ref -> string list -> string -> unit
  val ctrl_to_local_type : Controlang.ctrlType -> string -> string
end

