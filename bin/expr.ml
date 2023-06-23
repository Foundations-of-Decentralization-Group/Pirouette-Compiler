type expr =
  | Value of int
  | ChoreoVars of string
  | Variable of string
  | Condition of {lft: expr; op: string; rght: expr}
  | Branch of {ift: expr; thn : expr; el: expr}
  | Sync of {sndr: string; d: string; rcvr: string; thn: expr}
  | Fun of {name: string; arg: expr; body: expr}
  | Calling of {name: string; arg: expr}
  | Snd of {sndr: expr; name: string}
  | Let of {fst: expr; snd: expr; thn: expr}
  | Map of {name: string; arg: expr}
  | Assoc of {loc: string; arg: expr}
  | Abstraction of { param : string; body : expr }
  | Application of { funct : expr; argument : expr }
  | Comm_S of {sndr: expr; rcvr : expr}
  | Plus of {lft: expr; rght: expr}
  | Minus of {lft: expr; rght: expr}
  | Product of {lft: expr; rght: expr}
  | Division of {lft: expr; rght: expr}
  | UMinus of {sub_expr : expr}
[@@deriving show]