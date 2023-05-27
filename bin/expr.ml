type expr =
  | Value of int
  | Variable of string
  | Map of {name: string; arg: expr}
  | Sndr of {name: string; arg: expr}
  | Rcvr of {name: string; arg: expr}
  | Abstraction of { param : string; body : expr }
  | Application of { funct : expr; argument : expr }
  | Comm_S of {sndr: expr; rcvr : expr}
[@@deriving show]