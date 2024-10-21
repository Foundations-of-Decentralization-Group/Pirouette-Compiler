module Choreo = Choreo.M
module Local = Local.M

module M = struct
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
    | Send of expr * Local.loc_id
    | Recv of Local.loc_id
    | ChooseFor of Local.sync_label * Local.loc_id * expr
    | AllowChoice of Local.loc_id * (Local.sync_label * expr) list
    | FunDef of Local.pattern list * expr
    | FunApp of expr * expr
    | Pair of expr * expr
    | Fst of expr
    | Snd of expr
    | Left of expr
    | Right of expr
    | Match of expr * (Local.pattern * expr) list

  and stmt =
    | Decl of Local.pattern * typ
    | Assign of Local.pattern list * expr
    | TypeDecl of Local.typ_id * typ

  and stmt_block = stmt list
end

module With (Info : sig
    type t
  end) =
struct
  type 'a with_info = 'a * Info.t
  type typ = M.typ with_info
  type expr = M.expr with_info
  type stmt = M.stmt with_info
  type stmt_block = stmt list

  let info_of : 'a with_info -> Info.t = fun (_, i) -> i
end
