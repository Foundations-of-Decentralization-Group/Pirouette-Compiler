type expr =
  | Variable of string
  | Abstraction of { param : string; body : expr }
  | Application of { funct : expr; argument : expr }
[@@deriving show]