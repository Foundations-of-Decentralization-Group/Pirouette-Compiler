(* name of string 
   refactoring use ADT
   break expr -> l_expr and expr*)
   
   type location = Location of string
   [@@deriving show]
   
   type name = Name of string
   [@@deriving show]
   
   type direction = Direction of string
   [@@deriving show]
   
   type binop = 
     | Gt
     | Lt
     | Eq
   [@@deriving show]
   
   type localType =
     | IntType
     | StringType
     | BoolType
   [@@deriving show]
   
   type globalType =
     | DotType of location * localType
     | ArrowType of globalType * globalType
   [@@deriving show]
   
   let localType_equal a b = (match (a, b) with
     | (IntType, IntType) -> true
     | (StringType, StringType) -> true
     | (BoolType, BoolType) -> true
     | _ -> false
   )
   
   let rec globalType_equal a b = (match (a, b) with
     | (DotType (loc1, lt1), DotType (loc2, lt2)) -> loc1 = loc2 && localType_equal lt1 lt2
     | (ArrowType (gt11, gt12), ArrowType (gt21, gt22)) ->
         globalType_equal gt11 gt21 && globalType_equal gt12 gt22
     | _ -> false
   )
   
   type l_expr = 
     | INT of int
     | STRING of string
     | BOOL of bool
     | Variable of name * localType option
     | Condition of l_expr * binop * l_expr * localType option 
     | Plus of l_expr * l_expr * localType option
     | Minus of l_expr * l_expr * localType option
     | Product of l_expr * l_expr * localType option
     | Division of l_expr * l_expr * localType option
   [@@deriving show]
   
   type expr =
     | ChoreoVars of name * globalType option
     | Branch of expr * expr * expr * globalType option
     | Sync of location * direction * location * expr * globalType option
     | FunG of name * expr * expr * globalType option
     | FunL of name * location * l_expr * expr * globalType option
     | Calling of name * expr * globalType option
     | Snd of expr * location * globalType option
     | Let of location * l_expr * expr * expr * globalType option
     | Assoc of location * l_expr * globalType option
     | Application of expr * expr * globalType option
   [@@deriving show]
   