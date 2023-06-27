type localType = 
  | INT of int
  | STRING of string
  | BOOL of bool
[@@deriving show]

type globalType = 
  | DotType of {loc: string; typ: localType}
  | ArrowType of {ityp: globalType; otyp: globalType}
[@@deriving show]

type expr =
  | ChoreoVars of {name: string; typ: globalType option}
  | Variable of {name: string; typ: localType option}
  | Condition of {lft: expr; op: string; rght: expr; typ: localType option}
  | Branch of {ift: expr; thn : expr; el: expr; typ: globalType option}
  | Sync of {sndr: string; d: string; rcvr: string; thn: expr; typ: globalType option}
  | Fun of {name: string; arg: expr; body: expr; typ: globalType option}
  | Calling of {name: string; arg: expr; typ: globalType option}
  | Snd of {sndr: expr; name: string; typ: globalType option}
  | Let of {fst: expr; snd: expr; thn: expr; typ: globalType option}
  | Map of {name: string; arg: expr; typ: globalType option}
  | Assoc of {loc: string; arg: expr; typ: globalType option}
  | Application of { funct : expr; argument : expr; typ: globalType option }
  | Plus of {lft: expr; rght: expr; typ: localType option}
  | Minus of {lft: expr; rght: expr; typ: localType option}
  | Product of {lft: expr; rght: expr; typ: localType option}
  | Division of {lft: expr; rght: expr; typ: localType option}
[@@deriving show]