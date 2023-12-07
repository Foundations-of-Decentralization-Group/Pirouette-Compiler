module rec L_P_TREE : sig

  module Var : sig
    type t = Var of string
  end

  module Val : sig
    type t = 
      | Int of int
      | String of string
      | Bool of bool
  end

  module LPattern : sig
    type t =
      | Left of t
      | Right of t
      | Val of Val.t
      | Pair of (t * t)
      | Var of L_P_TREE.Var.t
      | Wild
  end

  module LocalAssignment: sig
    type t = 
      | LVarAsgn of {assign: (Var.t * t); 
          expr: L_P_TREE.LocalExpr.t}
  end

  module Operation : sig
    type t = 
      | Add of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Subtract of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Product of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Divide of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Lt of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Gt of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Lte of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Gte of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Eq of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Neq of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | And of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
      | Or of (L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t)
  end 

  module Pair : sig
    type t = L_P_TREE.LocalExpr.t * L_P_TREE.LocalExpr.t
  end

  module Match : sig
    type t = {
      matcher: t;
      branches: (LPattern.t * t) list
    }
  end

  module LocalExpr : sig
    type t = 
      | Unit
      | Val of Val.t
      | Var of Var.t
      | Binop of Operation.t
      | Let of {
          assignment: LocalAssignment.t;
          in_: t
        }
      | Pair of Pair.t
      | Fst of Pair.t
      | Snd of Pair.t 
      | Match of Match.t
      | Left of Match.t
      | Right of Match.t
  end
end = L_P_TREE