type typ =
  | TUnit
  | TLoc of Local.typ
  | TSend of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ

type expr =
  | Unit
  | Var of Local.var_id
  | FunDef of Local.var_id * expr
  | FunApp of expr * expr
  | If of expr * expr * expr
  | Match of expr * Local.pattern * expr
  | Ret of Local.expr
  | Let of Local.var_id * expr * expr
  | SendExpr of Local.expr * Local.loc_id * expr
  | RecvVar of Local.loc_id * Local.var_id * expr
  | Choose of Local.sync_label * Local.loc_id * expr
  | Choice of Local.loc_id * (Local.sync_label * expr) list
  | Seq of expr list
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr