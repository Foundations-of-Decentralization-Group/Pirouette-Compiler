type loc = Lexing.position

type location = LocId of string

type sync_label = Label of string

type var = VarId of string

type fn = FunId of string

type bin_op =
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or

type value =
  | Int of int
  | String of string
  | Bool of bool

type local_type = 
  | Unit of unit
  | Int of int
  | Bool of bool

type choreo_type =
  | Unit of unit
  | TLoc of location * local_type
  | TMap of choreo_type * choreo_type
  | TProd of choreo_type * choreo_type
  | TSum of choreo_type * choreo_type

type network_type =
  | Unit of unit
  | TMap of network_type * network_type
  | TProd of network_type * network_type
  | TSum of network_type * network_type

type local_pattern =
  | Default of unit
  | Val of value
  | Var of var
  | Pair of local_pattern * local_pattern
  | Left of local_pattern
  | Right of local_pattern

type pattern =
  | Default of unit
  | Var of var
  | Pair of pattern * pattern
  | LPatt of location * local_pattern
  | Left of pattern
  | Right of pattern

type declaration = 
  | DFun of fn * choreo_type * choreo_type
  | DVar of var * choreo_type
  | DLocVar of location * var * local_type
  | DType of string * choreo_type

type local_expr =
  | Nop of unit
  | Int of int
  | Val of value
  | Var of var
  | BinOp of bin_op * local_expr * local_expr

type choreo_expr =
  | Nop of unit
  | Var of var
  | If of choreo_expr * choreo_expr * choreo_expr
  | Case of choreo_expr * pattern * choreo_expr
  | Decl of decl_block * choreo_expr
  | FunDef of fn * choreo_expr
  | LocExpr of location * local_expr
  | LocExprMap of location * local_expr * location * var * choreo_expr
  | ExprMap of choreo_expr * location
  | LabelMap of location * sync_label * location
  | List of choreo_expr list
  | Pair of choreo_expr * choreo_expr
  | First of choreo_expr
  | Second of choreo_expr
  | Left of choreo_expr
  | Right of choreo_expr

and assignment = 
  | AVar of var * choreo_expr
  | AFun of fn * choreo_expr * pattern list
  | ALocVar of location * var * local_expr
  
and decl_block =
  | DDecl of declaration list
  | DAssign of assignment * decl_block

type network_expr =
  | Nop of unit
  | Var of var
  | If of network_expr * network_expr * network_expr
  | Case of network_expr * pattern * network_expr
  | Ret of local_expr
  | Let of var * network_expr * network_expr
  | SendExpr of local_expr * location * network_expr
  | RecvVar of location * var * network_expr
  | Choose of sync_label * location * network_expr
  | Choice of location * (sync_label * network_expr) list
  | List of network_expr list
  | Pair of network_expr * network_expr
  | First of network_expr
  | Second of network_expr
  | Left of network_expr
  | Right of network_expr

(* Placeholder for the parser *)
type atom =
  | Int of int
  | Float of float
  | Word of string

type program = decl_block
(* type program = Program of atom list *)
