type typ =
  | TUnit
  | TLoc of Local.typ
  | TMap of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ

type expr =
  | Unit
  | Var of Local.var_id
  | Ret of Local.expr
  | If of expr * expr * expr
  | Let of stmt list * expr
  | SendTo of expr * Local.loc_id
  | RecvFrom of Local.loc_id
  | ChooseFor of Local.sync_label * Local.loc_id * expr
  | AllowChoice of Local.loc_id * (Local.sync_label * expr) list
  | FunDef of Local.pattern * expr
  | FunApp of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * (Local.pattern * expr) list

and stmt =
  | Decl of Local.pattern * typ
  | Assign of Local.pattern * expr
  | TypeDecl of Local.typ_id * typ

and stmt_block = stmt list

type program = Prog of stmt list