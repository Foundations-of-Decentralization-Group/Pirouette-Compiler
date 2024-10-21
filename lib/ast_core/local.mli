module M : sig
  type value =
    | Int of int
    | String of string
    | Bool of bool

  type loc_id = LocId of string
  type var_id = VarId of string
  type typ_id = TypId of string
  type sync_label = LabelId of string

  type un_op =
    | Not
    | Neg

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

  type typ =
    | TUnit
    | TInt
    | TString
    | TBool
    | TProd of typ * typ
    | TSum of typ * typ

  type pattern =
    | Default
    | Val of value
    | Var of var_id
    | Pair of pattern * pattern
    | Left of pattern
    | Right of pattern

  type expr =
    | Unit
    | Val of value
    | Var of var_id
    | UnOp of un_op * expr
    | BinOp of expr * bin_op * expr
    | Let of var_id * expr * expr
    | Pair of expr * expr
    | Fst of expr
    | Snd of expr
    | Left of expr
    | Right of expr
    | Match of expr * (pattern * expr) list
end

module With : functor
    (Info : sig
       type t
     end)
    -> sig
  type value = M.value * Info.t
  type loc_id = M.loc_id * Info.t
  type var_id = M.var_id * Info.t
  type typ_id = M.typ_id * Info.t
  type sync_label = M.sync_label * Info.t
  type un_op = M.un_op * Info.t
  type bin_op = M.bin_op * Info.t
  type typ = M.typ * Info.t
  type pattern = M.pattern * Info.t
  type expr = M.expr * Info.t

  val info_of : 'a * 'b -> 'b
end
