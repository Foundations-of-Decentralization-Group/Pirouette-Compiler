module rec L_P_TREE : sig
  type var = Var of string

  and value =
    | Int of int
    | String of string
    | Bool of bool

  and lpattern =
    | Left of lpattern
    | Right of lpattern
    | Val of value
    | Pair of (lpattern * lpattern)
    | Var of var
    | Wild

  and local_assignment =
    | LVarAsgn of { assign: (var * lpattern); expr: local_expr }

  and operation =
    | Add of (local_expr * local_expr)
    | Subtract of (local_expr * local_expr)
    | Product of (local_expr * local_expr)
    | Divide of (local_expr * local_expr)
    | Lt of (local_expr * local_expr)
    | Gt of (local_expr * local_expr)
    | Lte of (local_expr * local_expr)
    | Gte of (local_expr * local_expr)
    | Eq of (local_expr * local_expr)
    | Neq of (local_expr * local_expr)
    | And of (local_expr * local_expr)
    | Or of (local_expr * local_expr)

  and pair = local_expr * local_expr

  and match_expr = {
    matcher: match_expr;
    branches: (lpattern * match_expr) list
  }

  and local_expr =
    | Unit
    | Val of value
    | Var of var
    | Binop of operation
    | Let of {
        assignment: local_assignment;
        in_: local_expr
      }
    | Pair of pair
    | Fst of pair
    | Snd of pair
    | Match of match_expr
    | Left of match_expr
    | Right of match_expr
end = L_P_TREE