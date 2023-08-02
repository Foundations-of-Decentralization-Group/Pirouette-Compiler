type location = Location of string
[@@deriving show]

let compare_location loc1 loc2 =
  match (loc1, loc2) with
  | (Location s1, Location s2) -> compare s1 s2

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



module LocalMap = Map.Make(String)

module ImmutableMap = struct   
  include LocalMap
  type local_map = localType LocalMap.t
end

let get_local_map (typeMap: localType LocalMap.t ImmutableMap.t) (Location loc: location) : localType LocalMap.t = 
  (match ImmutableMap.find_opt loc typeMap with
   | Some localmap -> localmap
   | None -> LocalMap.empty
  ) 

let add_map (typeMap: localType LocalMap.t ImmutableMap.t) (Location loc: location) (arg: string) (typ: localType) : localType LocalMap.t ImmutableMap.t = 
  let nestedMap = 
    try
      ImmutableMap.find loc typeMap
    with Not_found -> ImmutableMap.empty
  in
  let updatedNestedMap = ImmutableMap.add arg typ nestedMap in
  ImmutableMap.add loc updatedNestedMap typeMap

let find_map (typeMap: localType LocalMap.t ImmutableMap.t) (loc: location) (arg: string) : globalType option = 
  let localMap = get_local_map typeMap loc in
  if LocalMap.is_empty localMap then
    None
  else
    (match LocalMap.find_opt arg localMap with
    | Some typ -> Some (DotType(loc, typ))
    | None -> None
    ) 

module ChoreoMap = Map.Make(String)



(* let myNestedMap = NestedMap.add "key1" 10 NestedMap.empty
let mainMap = MainMap.add "outer_key" myNestedMap MainMap.empty *)


