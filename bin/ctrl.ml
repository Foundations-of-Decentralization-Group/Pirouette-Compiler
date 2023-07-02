open Basictypes

type ctrl =
  | INT of int
  | STRING of string
  | BOOL of bool
  | ChoreoVars of name * globalType
  | Variable of name * localType
  | Ret of ctrl * localType
  | Unit
  | Snd of ctrl * location * ctrl * localType
  | Rcv of ctrl * location * ctrl * localType
  | Branch of ctrl * ctrl * ctrl * localType
  | Choose of direction * location * ctrl * localType
  | AllowL of location * ctrl * localType
  | AllowR of location * ctrl * localType
  | AllowLR of location * ctrl * ctrl * localType
  | Let of ctrl * ctrl * ctrl * localType
  | Fun of name * ctrl * ctrl * localType
  | Calling of name * ctrl * localType
  | Application of ctrl * ctrl * localType
  | Condition of ctrl * binop * ctrl * localType
  | Plus of ctrl * ctrl * localType
  | Minus of ctrl * ctrl * localType
  | Product of ctrl * ctrl * localType
  | Division of ctrl * ctrl * localType
[@@deriving show]