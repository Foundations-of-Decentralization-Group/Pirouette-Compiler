open Local

type network_type =
  | TUnit
  | TLoc of local_type
  | TSend of network_type * network_type
  | TProd of network_type * network_type
  | TSum of network_type * network_type

type network_expr =
  | Unit
  | Var of var_id
  | FunDef of var_id * network_expr
  | FunApp of network_expr * network_expr
  | If of network_expr * network_expr * network_expr
  | Match of network_expr * local_pattern * network_expr
  | Ret of local_expr
  | Let of var_id * network_expr * network_expr
  | SendExpr of local_expr * loc_id * network_expr
  | RecvVar of loc_id * var_id * network_expr
  | Choose of sync_label * loc_id * network_expr
  | Choice of loc_id * (sync_label * network_expr) list
  | Seq of network_expr list
  | Pair of network_expr * network_expr
  | Fst of network_expr
  | Snd of network_expr
  | Left of network_expr
  | Right of network_expr