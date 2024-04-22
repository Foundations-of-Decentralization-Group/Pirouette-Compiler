type typ =
  | TUnit
  | TLoc of Local.loc_id * Local.typ
  | TMap of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ
  (* | TCustom Local.typ_id *)

type pattern =
  | Default
  | Var of Local.var_id
  | Pair of pattern * pattern
  | LocPatt of Local.loc_id * Local.pattern
  | Left of pattern
  | Right of pattern

type expr =
  | Unit
  | Var of Local.var_id
  | LocExpr of Local.loc_id * Local.expr
  | Send of expr * Local.loc_id * Local.loc_id
  | Sync of Local.loc_id * Local.sync_label * Local.loc_id * expr
  | If of expr * expr * expr
  | Let of stmt_block * expr
  | FunDef of pattern * expr
  | FunApp of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * (pattern * expr) list

and stmt =
  | Decl of pattern * typ
  | Assign of pattern * expr
  | TypeDecl of Local.typ_id * typ

and stmt_block = stmt list

type program = Prog of stmt_block
