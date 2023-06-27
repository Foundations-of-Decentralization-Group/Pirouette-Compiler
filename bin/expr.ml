type expr =
  | INT of int
  | STRING of string
  | BOOL of bool
  | ChoreoVars of string
  | Variable of {name: string; typ: string option}
  | Condition of {lft: expr; op: string; rght: expr; typ: string option}
  | Branch of {ift: expr; thn : expr; el: expr; typ: string option}
  | Sync of {sndr: string; d: string; rcvr: string; thn: expr; typ: string option}
  | Fun of {name: string; arg: expr; body: expr; typ: string option}
  | Calling of {name: string; arg: expr; typ: string option}
  | Snd of {sndr: expr; name: string; typ: string option}
  | Let of {fst: expr; snd: expr; thn: expr; typ: string option}
  | Map of {name: string; arg: expr; typ: string option}
  | Assoc of {loc: string; arg: expr; typ: string option}
  | Application of { funct : expr; argument : expr; typ: string option }
  | Plus of {lft: expr; rght: expr; typ: string option}
  | Minus of {lft: expr; rght: expr; typ: string option}
  | Product of {lft: expr; rght: expr; typ: string option}
  | Division of {lft: expr; rght: expr; typ: string option}
[@@deriving show]