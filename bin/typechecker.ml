open Expr
open Containers

exception TypeCheckingFailedException of string

type tc_ast = 
  | GTcast of globalType option * expr option
  | LTcast of localType option * l_expr option

module ImmutableMap = Map.Make(String)

(* let my_map = ImmutableMap.empty
let new_map = ImmutableMap.add 1 "one" my_map
let _updated_map = ImmutableMap.add 2 "two" new_map

let () =
  ImmutableMap.iter (fun k v -> Printf.printf "%d -> %s\n" k v) new_map *)

let rec local_type_check (l_expr_ast: l_expr) = 
  match l_expr_ast with
  | STRING s -> LTcast(Some StringType, Some (STRING s))
  | INT i -> LTcast(Some IntType, Some (INT i))
  | BOOL b -> LTcast(Some BoolType, Some (BOOL b))
  | Variable(name, typ) -> LTcast(typ, Some (Variable (name, typ)))
  | Condition (lft, op, rght, _) -> 
    (match local_type_check lft, local_type_check rght with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some BoolType, Some (Condition (ast1, op, ast2, Some BoolType)))
          | _ -> raise (TypeCheckingFailedException "Condition expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Condition") 
    )
  | Plus (lft, rght, _) -> 
    (match local_type_check lft, local_type_check rght with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Plus (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Plus expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Plus")
      )
  | Minus (lft, rght, _) -> 
    (match local_type_check lft, local_type_check rght with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Minus (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Minus expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Minus")
      )
  | Division (lft, rght, _) -> 
    (match local_type_check lft, local_type_check rght with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Division (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Division expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Division")
      )
  | Product (lft, rght, _) -> 
    (match local_type_check lft, local_type_check rght with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Product (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Product expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Product")
      )
  | _ -> raise (TypeCheckingFailedException "Not executed")

let rec type_check (expr_ast: expr) (typeMap: globalType ImmutableMap.t): tc_ast =
  match expr_ast with
    | ChoreoVars (Name name, typ) -> GTcast(typ, Some (ChoreoVars (Name name, typ)))
    | Branch (ift, thn, el, _ ) ->
      (match type_check ift typeMap, type_check thn typeMap, type_check el typeMap with
        | GTcast(Some DotType(_, BoolType), Some if_ast), GTcast(Some thn_typ, Some thn_ast), 
          GTcast(Some el_typ, Some el_ast) when globalType_equal thn_typ el_typ -> 
            GTcast(Some thn_typ, Some (Branch (if_ast, thn_ast, el_ast,Some thn_typ)))
        | _ -> raise (TypeCheckingFailedException "If should contain an expression that resolves to boolean and both branches should evaluate to same type.") 
      )
    | Assoc (loc, Variable(Name name, typ), _) ->
      (match local_type_check (Variable(Name name, typ)) with 
        | LTcast(Some typ1, Some ast1) -> 
              GTcast( Some (DotType( loc, typ1)), Some (Assoc(loc, ast1, Some (DotType(loc, typ1)))))
        | LTcast(None, Some Variable(Name l_name, _)) -> 
          let Location loc_str = loc in
          (match ImmutableMap.find_opt (loc_str^"."^name) typeMap with
            | Some (DotType(l, typ)) -> GTcast(Some (DotType(l, typ)), Some(Assoc (loc, Variable(Name l_name, Some typ), Some (DotType(l, typ)))))
            | Some _ -> raise (TypeCheckingFailedException "Assoc can only have P.t type")
            | None -> raise (TypeCheckingFailedException "Variable type is not declared")
          ) 
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Assoc (loc, arg, _) ->
      (match local_type_check arg with 
        | LTcast(Some typ1, Some ast1) -> 
          GTcast(Some (DotType(loc, typ1)), Some (Assoc (loc, ast1, Some (DotType(loc, typ1)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Snd (Assoc(loc, arg, typ), rcvng_location, _) ->
        (match type_check (Assoc(loc, arg, typ)) typeMap with 
          | GTcast(Some DotType(_, assoc_typ), Some assoc_ast) -> 
              GTcast(Some (DotType(rcvng_location, assoc_typ)), 
                Some (Snd(assoc_ast, rcvng_location, Some (DotType(rcvng_location, assoc_typ)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd")
        )
    | Snd (_, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd | sndr can only be Assoc")    
    | Let (Location loc, Variable(Name bndr, Some bndr_typ), snd, thn, _) -> 
      let _new_map = ImmutableMap.add (loc ^"."^ bndr) (DotType(Location loc, bndr_typ)) typeMap in
      (match type_check snd _new_map, type_check thn _new_map with
        | GTcast(Some DotType(_, snd_typ), Some snd_ast), GTcast(Some thn_typ, Some thn_ast) 
          when localType_equal snd_typ bndr_typ ->
            GTcast(Some thn_typ, Some (Let (Location loc, Variable(Name bndr, Some bndr_typ), snd_ast, thn_ast, Some thn_typ)))
        | _ -> raise (TypeCheckingFailedException "Variable should be assigned value of same type")
      ) 
    | Let (_, _, _, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Let")    
    | Sync (Location sndr, Direction d, Location rcvr, thn, _) ->
      (match type_check thn typeMap with
        | GTcast(Some thn_typ, Some thn_ast) -> 
          GTcast(Some thn_typ, Some (Sync (Location sndr, Direction d, Location rcvr, thn_ast, Some thn_typ)))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Sync")
      )
    | Application (funct, arg, _) ->
      (match type_check funct typeMap, type_check arg typeMap with
        | GTcast(Some (ArrowType(ityp, otyp)), Some fun_ast), GTcast(Some arg_typ, Some arg_ast) 
        when globalType_equal ityp arg_typ ->
          GTcast(Some otyp, Some (Application(fun_ast, arg_ast, Some otyp)))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Application")
      )
    |FunG (name, arg, body, Some typ) ->
      (match type_check arg typeMap, type_check body typeMap with
        | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) 
          when globalType_equal (typ) (ArrowType(arg_typ, body_typ))->
          GTcast(Some (ArrowType(arg_typ, body_typ)), Some (FunG (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
        | GTcast(_, _), GTcast(_, _) ->
          raise (TypeCheckingFailedException "FunG type mismatch")
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating FunG with type defined")
      )
    |FunG (name, arg, body, None) ->
      (match type_check arg typeMap, type_check body typeMap with
        | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) ->
          GTcast(Some (ArrowType(arg_typ, body_typ)), Some (FunG (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating FunG without type declaration")
      )
    |FunL (name, Location loc, Variable(Name bndr, Some bndr_typ), body, Some typ) ->
      let _new_map = ImmutableMap.add (loc ^"."^ bndr) (DotType(Location loc, bndr_typ)) typeMap in
      (match type_check body _new_map with
        | GTcast(Some body_typ, Some body_ast) 
          when globalType_equal (typ) (ArrowType(DotType(Location loc, bndr_typ), body_typ))->
          GTcast(Some (ArrowType(DotType(Location loc, bndr_typ), body_typ)), 
          Some (FunL (name, Location loc, Variable(Name bndr, Some bndr_typ), body_ast, 
          Some (ArrowType(DotType(Location loc, bndr_typ), body_typ)))))
        | GTcast(_, _) ->
          raise (TypeCheckingFailedException "FunL type mismatch")
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating FunL with type defined")
      )
    |FunL (name, loc, Variable(Name bndr, Some bndr_typ), body, None) ->
      (match type_check body typeMap with
        | GTcast(Some body_typ, Some body_ast) ->
          GTcast(Some (ArrowType(DotType(loc, bndr_typ), body_typ)), 
          Some (FunL (name, loc, Variable(Name bndr, Some bndr_typ), body_ast, 
          Some (ArrowType(DotType(loc, bndr_typ), body_typ)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating FunL without type defined")
      )
    (* | Calling *)
    | _ -> raise (TypeCheckingFailedException "No Case matched") 


let _ast = (Expr.Assoc ((Expr.Location "person"),
(Expr.Variable ((Expr.Name "name"), (Some Expr.StringType))), None))

let ast2 = (Expr.FunG ((Expr.Name "fname"),    
(Expr.ChoreoVars ((Expr.Name "X"),
   (Some (Expr.DotType ((Expr.Location "person1"), Expr.IntType))))),
(Expr.Assoc ((Expr.Location "person1"),
   (Expr.Variable ((Expr.Name "name"), (Some Expr.IntType))), None)),
(Some (Expr.ArrowType (
         (Expr.DotType ((Expr.Location "person1"), Expr.IntType)),
         (Expr.DotType ((Expr.Location "person1"), Expr.IntType)))))
))
(* let ast = Expr.Plus {lft = (Expr.INT 3); rght = (Expr.INT 5);
typ = (Some typ_int)} *)

let () = 
  let typeMap = ImmutableMap.empty in
  let res = type_check ast2 typeMap in
  let r = match res with
    | GTcast(_, Some ast1) -> show_expr ast1
    | _ -> ""
  in Printf.printf "%s\n\n" r