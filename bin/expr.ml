type expr =
  | Value of int
  | ChoreoVars of string
  | Variable of string
  | Op of {lft: expr; op: string; rght: expr}
  | Condition of {lft: expr; op: string; rght: expr}
  | Branch of {ift: expr; thn : expr; el: expr}
  | Sync of {sndr: string; d: string; rcvr: string}
  | Fun of {name: string; arg: expr; body: expr}
  | Seq of {fst: expr; thn : expr}
  | Snd of {sndr: expr; name: string}
  | Let of {fst: expr; snd: expr; thn: expr}
  | Map of {name: string; arg: expr}
  | Assoc of {loc: string; arg: expr}
  | Abstraction of { param : string; body : expr }
  | Application of { funct : expr; argument : expr }
  | Comm_S of {sndr: expr; rcvr : expr}
  | None
[@@deriving show]