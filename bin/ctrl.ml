open Basictypes

type l_ctrl = 
  | INT of int
  | STRING of string
  | BOOL of bool
  | Variable of name * ctrlType
  | Condition of l_ctrl * binop * l_ctrl * ctrlType
  | Plus of l_ctrl * l_ctrl * ctrlType
  | Minus of l_ctrl * l_ctrl * ctrlType
  | Product of l_ctrl * l_ctrl * ctrlType
  | Division of l_ctrl * l_ctrl * ctrlType
[@@deriving show]

type ctrl =
  | Unit
  | Ret of l_ctrl * ctrlType
  | ChoreoVars of name * ctrlType
  | Snd of l_ctrl * location * ctrl * ctrlType
  | Rcv of l_ctrl * location * ctrl * ctrlType
  | Branch of ctrl * ctrl * ctrl * ctrlType
  | Choose of direction * location * ctrl * ctrlType
  | AllowL of location * ctrl * ctrlType
  | AllowR of location * ctrl * ctrlType
  | AllowLR of location * ctrl * ctrl * ctrlType
  | Let of ctrl * ctrl * ctrl * ctrlType
  | Fun of name * ctrl * ctrl * ctrlType
  | Calling of name * ctrl * ctrlType
  | Application of ctrl * ctrl * ctrlType
[@@deriving show]