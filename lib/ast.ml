type pos = Lexing.position

type loc_name = LocId of string
type var_name = VarId of string
type fn_name = FunId of string
type type_name = TypeId of string
type sync_label = LabelId of string

type bin_op =
  | Plus
  | Minus
  | Times
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

type value =
  | Int of int
  | String of string
  | Bool of bool

type local_type = 
  | TUnit
  | TInt
  | TString
  | TBool
  | TProd of local_type * local_type
  | TSum of local_type * local_type

type choreo_type =
  | TUnit
  | TLoc of loc_name * local_type
  | TSend of choreo_type * choreo_type
  | TProd of choreo_type * choreo_type
  | TSum of choreo_type * choreo_type

type network_type =
  | TUnit
  | TSend of network_type * network_type
  | TProd of network_type * network_type
  | TSum of network_type * network_type

type local_pattern =
  | Default
  | Val of value
  | Var of var_name
  | Pair of local_pattern * local_pattern
  | Left of local_pattern
  | Right of local_pattern

type pattern =
  | Default
  | Var of var_name
  | Pair of pattern * pattern
  | LocPatt of loc_name * local_pattern
  | Left of pattern
  | Right of pattern

type declaration = 
  | DVar of var_name * choreo_type
  | DFun of fn_name * choreo_type * choreo_type
  | DLocVar of loc_name * var_name * local_type
  | DType of type_name * choreo_type

type local_expr =
  | Nop
  | Int of int
  | Val of value
  | Var of var_name
  | BinOp of local_expr * bin_op * local_expr
  | Match of local_expr * (local_pattern * local_expr) list
  | Let of var_name * local_expr * local_expr
  | Pair of local_expr * local_expr
  | Fst of local_expr
  | Snd of local_expr
  | Left of local_expr
  | Right of local_expr

type choreo_expr =
  | Nop
  | Var of var_name
  | LocExpr of loc_name * local_expr
  | LocExprSend of loc_name * local_expr * loc_name * var_name * choreo_seq
  | ExprSend of choreo_seq * loc_name
  | LabelSend of loc_name * sync_label * loc_name* choreo_seq
  | FunDef of fn_name * choreo_seq
  | If of choreo_seq * choreo_seq * choreo_seq
  | Match of choreo_seq * (pattern * choreo_seq) list
  | Let of decl_block * choreo_seq
  | Pair of choreo_seq * choreo_seq
  | Fst of choreo_seq
  | Snd of choreo_seq
  | Left of choreo_seq
  | Right of choreo_seq

and choreo_seq = choreo_expr list

and assignment = 
  | AVar of var_name * choreo_seq
  | AFun of fn_name * pattern list * choreo_seq
  | ALocVar of loc_name * var_name * choreo_seq

and decl_or_assign =
  | Decl of declaration
  | Assign of assignment

and decl_block = decl_or_assign list

type network_expr =
  | Nop
  | Var of var_name
  | If of network_seq * network_seq * network_seq
  | Match of network_seq * pattern * network_seq
  | Ret of local_expr
  | Let of var_name * network_seq * network_seq
  | SendExpr of local_expr * loc_name * network_seq
  | RecvVar of loc_name * var_name * network_seq
  | Choose of sync_label * loc_name * network_seq
  | Choice of loc_name * (sync_label * network_seq) list
  | Seq of network_seq list
  | Pair of network_seq * network_seq
  | Fst of network_seq
  | Snd of network_seq
  | Left of network_seq
  | Right of network_seq

and network_seq = network_expr list

type program = Prog of decl_block