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

module With_meta (Info : sig
    type t
  end) =
struct
  type value =
    | Int of int * Info.t
    | String of string * Info.t
    | Bool of bool * Info.t

  type loc_id = LocId of string * Info.t
  type var_id = VarId of string * Info.t
  type typ_id = TypId of string * Info.t
  type sync_label = LabelId of string * Info.t

  type un_op =
    | Not of Info.t
    | Neg of Info.t

  type bin_op =
    | Plus of Info.t
    | Minus of Info.t
    | Times of Info.t
    | Div of Info.t
    | And of Info.t
    | Or of Info.t
    | Eq of Info.t
    | Neq of Info.t
    | Lt of Info.t
    | Leq of Info.t
    | Gt of Info.t
    | Geq of Info.t

  type typ =
    | TUnit of Info.t
    | TInt of Info.t
    | TString of Info.t
    | TBool of Info.t
    | TProd of typ * typ * Info.t
    | TSum of typ * typ * Info.t

  type pattern =
    | Default of Info.t
    | Val of value * Info.t
    | Var of var_id * Info.t
    | Pair of pattern * pattern * Info.t
    | Left of pattern * Info.t
    | Right of pattern * Info.t

  type expr =
    | Unit of Info.t
    | Val of value * Info.t
    | Var of var_id * Info.t
    | UnOp of un_op * expr * Info.t
    | BinOp of expr * bin_op * expr * Info.t
    | Let of var_id * expr * expr * Info.t
    | Pair of expr * expr * Info.t
    | Fst of expr * Info.t
    | Snd of expr * Info.t
    | Left of expr * Info.t
    | Right of expr * Info.t
    | Match of expr * (pattern * expr) list * Info.t
end
