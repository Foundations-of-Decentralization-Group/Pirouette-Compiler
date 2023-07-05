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

type ctrlType = 
  | Int
  | String
  | Bool
  | CtrlFun of ctrlType * ctrlType

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


