open Basictypes

type l_expr = 
  | INT of int
  | STRING of string
  | BOOL of bool
  | Variable of name * localType option
  | Condition of l_expr * binop * l_expr * localType option 
  | Plus of l_expr * l_expr * localType option
  | Minus of l_expr * l_expr * localType option
  | Product of l_expr * l_expr * localType option
  | Division of l_expr * l_expr * localType option
[@@deriving show]

type expr =
  | ChoreoVars of name * globalType option
  | Branch of expr * expr * expr * globalType option
  | Sync of location * direction * location * expr * globalType option
  | FunG of name * expr * expr * globalType option
  | FunL of name * location * l_expr * expr * globalType option
  | Calling of name * expr * globalType option
  | Snd of expr * location * globalType option
  | Let of location * l_expr * expr * expr * globalType option
  | Assoc of location * l_expr * globalType option
  | Application of expr * expr * globalType option
[@@deriving show]
   