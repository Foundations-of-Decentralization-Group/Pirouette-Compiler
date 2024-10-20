type typ =
  | TUnit
  | TLoc of Local.loc_id * Local.typ
  | TMap of typ * typ
  | TProd of typ * typ
  | TSum of typ * typ

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
  | Send of Local.loc_id * expr * Local.loc_id
  | Sync of Local.loc_id * Local.sync_label * Local.loc_id * expr
  | If of expr * expr * expr
  | Let of stmt_block * expr
  | FunDef of pattern list * expr
  | FunApp of expr * expr
  | Pair of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * (pattern * expr) list

and stmt =
  | Decl of pattern * typ
  | Assign of pattern list * expr (* list is only for F P1 P2 ... Pn := C *)
  | TypeDecl of Local.typ_id * typ

and stmt_block = stmt list

module With_meta (Info : sig
    type t
  end) =
struct
  type typ =
    | TUnit of Info.t
    | TLoc of Local.loc_id * Local.typ * Info.t
    | TMap of typ * typ * Info.t
    | TProd of typ * typ * Info.t
    | TSum of typ * typ * Info.t

  type pattern =
    | Default of Info.t
    | Var of Local.var_id * Info.t
    | Pair of pattern * pattern * Info.t
    | LocPatt of Local.loc_id * Local.pattern * Info.t
    | Left of pattern * Info.t
    | Right of pattern * Info.t

  type expr =
    | Unit of Info.t
    | Var of Local.var_id * Info.t
    | LocExpr of Local.loc_id * Local.expr * Info.t
    | Send of Local.loc_id * expr * Local.loc_id * Info.t
    | Sync of Local.loc_id * Local.sync_label * Local.loc_id * expr * Info.t
    | If of expr * expr * expr * Info.t
    | Let of stmt_block * expr * Info.t
    | FunDef of pattern list * expr * Info.t
    | FunApp of expr * expr * Info.t
    | Pair of expr * expr * Info.t
    | Fst of expr * Info.t
    | Snd of expr * Info.t
    | Left of expr * Info.t
    | Right of expr * Info.t
    | Match of expr * (pattern * expr) list * Info.t

  and stmt =
    | Decl of pattern * typ * Info.t
    | Assign of pattern list * expr * Info.t (* list is only for F P1 P2 ... Pn := C *)
    | TypeDecl of Local.typ_id * typ * Info.t

  and stmt_block = stmt list
end
