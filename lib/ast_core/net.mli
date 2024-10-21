module M : sig
  type typ =
    | TUnit
    | TLoc of Local.M.typ
    | TMap of typ * typ
    | TProd of typ * typ
    | TSum of typ * typ

  type expr =
    | Unit
    | Var of Local.M.var_id
    | Ret of Local.M.expr
    | If of expr * expr * expr
    | Let of stmt list * expr
    | Send of expr * Local.M.loc_id
    | Recv of Local.M.loc_id
    | ChooseFor of Local.M.sync_label * Local.M.loc_id * expr
    | AllowChoice of Local.M.loc_id * (Local.M.sync_label * expr) list
    | FunDef of Local.M.pattern list * expr
    | FunApp of expr * expr
    | Pair of expr * expr
    | Fst of expr
    | Snd of expr
    | Left of expr
    | Right of expr
    | Match of expr * (Local.M.pattern * expr) list

  and stmt =
    | Decl of Local.M.pattern * typ
    | Assign of Local.M.pattern list * expr
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
  type expr = M.expr with_info
  type stmt = M.stmt with_info
  type stmt_block = stmt list

  val info_of : 'a with_info -> Info.t
end
