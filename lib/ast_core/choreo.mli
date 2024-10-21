module M : sig
  type typ =
    | TUnit
    | TLoc of Local.M.loc_id * Local.M.typ
    | TMap of typ * typ
    | TProd of typ * typ
    | TSum of typ * typ

  type pattern =
    | Default
    | Var of Local.M.var_id
    | Pair of pattern * pattern
    | LocPatt of Local.M.loc_id * Local.M.pattern
    | Left of pattern
    | Right of pattern

  type expr =
    | Unit
    | Var of Local.M.var_id
    | LocExpr of Local.M.loc_id * Local.M.expr
    | Send of Local.M.loc_id * expr * Local.M.loc_id
    | Sync of Local.M.loc_id * Local.M.sync_label * Local.M.loc_id * expr
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
    | Assign of pattern list * expr
    | TypeDecl of Local.M.typ_id * typ

  and stmt_block = stmt list
end

module With : functor
    (Info : sig
       type t
     end)
    -> sig
  type 'a with_info = 'a * Info.t
  type typ = M.typ with_info
  type pattern = M.pattern with_info
  type expr = M.expr with_info
  type stmt = M.stmt with_info
  type stmt_block = stmt list

  val info_of : 'a with_info -> Info.t
end
