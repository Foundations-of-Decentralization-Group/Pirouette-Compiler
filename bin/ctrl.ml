type ctrl =
  | Value of int
  | ChoreoVars of string
  | Variable of string
  | Ret of {arg: ctrl}
  | Unit
  | Snd of {arg: ctrl; loc: string; thn: ctrl}
  | Rcv of {arg: ctrl; loc: string; thn: ctrl}
  | Branch of {ift: ctrl; thn : ctrl; el: ctrl}
  | Choose of {d: string; loc: string; thn: ctrl}
  | AllowL of {loc: string; thn: ctrl}
  | AllowR of {loc: string; thn: ctrl}
  | AllowLR of {loc: string; thnL: ctrl; thnR: ctrl}
  | Let of {binder: ctrl; arg: ctrl; thn: ctrl}
  | Fun of {name: string; arg: ctrl; body: ctrl}
  | Application of { funct : ctrl; argument : ctrl }
  | Condition of {lft: ctrl; op: string; rght: ctrl}
  | Map of {name: string; arg: ctrl}
  | Plus of {lft: ctrl; rght: ctrl}
  | Minus of {lft: ctrl; rght: ctrl}
  | Product of {lft: ctrl; rght: ctrl}
  | Division of {lft: ctrl; rght: ctrl}  
[@@deriving show]