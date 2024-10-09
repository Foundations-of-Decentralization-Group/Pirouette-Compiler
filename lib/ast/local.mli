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
