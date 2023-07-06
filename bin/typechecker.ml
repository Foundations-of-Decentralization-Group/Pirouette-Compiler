open Expr
open Containers
open Basictypes

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

let rec local_type_check (l_expr_ast: l_expr) (loc : location) (typeMap: globalType ImmutableMap.t) = 
  match l_expr_ast with
  | STRING s -> LTcast(Some StringType, Some (STRING s))
  | INT i -> LTcast(Some IntType, Some (INT i))
  | BOOL b -> LTcast(Some BoolType, Some (BOOL b))
  | Variable(Name name, typ) -> 
    let Location loc_str = loc in
    (match ImmutableMap.find_opt (loc_str^"."^name) typeMap, typ with
      | Some (DotType(Location _, inf_typ)), Some dec_typ 
        when localType_equal inf_typ dec_typ ->
        LTcast(Some dec_typ, Some (Variable (Name name, Some dec_typ)))
      | Some _, Some _ ->
        raise (TypeCheckingFailedException "inferred type and declared type do not match")
      | Some (DotType(Location _, inf_typ)), None ->
        LTcast(Some inf_typ, Some (Variable (Name name, Some inf_typ)))
      | None, Some dec_typ->
        LTcast(Some dec_typ, Some (Variable (Name name, Some dec_typ)))
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Variable")
    )

  | Condition (lft, op, rght, _) -> 
    (match local_type_check lft loc typeMap, local_type_check rght loc typeMap with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some BoolType, Some (Condition (ast1, op, ast2, Some BoolType)))
          | _ -> raise (TypeCheckingFailedException "Condition expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Condition") 
    )
  | Plus (lft, rght, _) -> 
    (match local_type_check lft loc typeMap, local_type_check rght loc typeMap with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Plus (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Plus expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Plus")
      )
  | Minus (lft, rght, _) -> 
    (match local_type_check lft loc typeMap, local_type_check rght loc typeMap with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Minus (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Minus expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Minus")
      )
  | Division (lft, rght, _) -> 
    (match local_type_check lft loc typeMap, local_type_check rght loc typeMap with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Division (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Division expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Division")
      )
  | Product (lft, rght, _) -> 
    (match local_type_check lft loc typeMap, local_type_check rght loc typeMap with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | IntType, IntType -> 
            LTcast(Some IntType, Some (Product (ast1, ast2, Some IntType)))
          | _ -> raise (TypeCheckingFailedException "Product expects left and right arguments to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Product")
    )

let rec type_check (expr_ast: expr) (typeMap: globalType ImmutableMap.t): tc_ast =
  match expr_ast with
    | ChoreoVars (Name name, typ) -> 
      (match ImmutableMap.find_opt (name) typeMap, typ with
        | Some inf_typ, Some dec_typ when globalType_equal inf_typ dec_typ -> 
          GTcast(Some inf_typ, Some (ChoreoVars (Name name, Some inf_typ)))
        | Some _, Some _ ->
          raise (TypeCheckingFailedException "Choreovar inferred and declared type does not match")
        | Some inf_typ, None ->
          GTcast(Some inf_typ, Some (ChoreoVars (Name name, Some inf_typ)))
        | None, Some dec_typ ->
          GTcast(Some dec_typ, Some (ChoreoVars (Name name, Some dec_typ)))
        | None, None ->
          raise (TypeCheckingFailedException "Choreovar type not declared and can not be inferred")
      )
    | Branch (ift, thn, el, _ ) ->
      (match type_check ift typeMap, type_check thn typeMap, type_check el typeMap with
        | GTcast(Some DotType(_, BoolType), Some if_ast), GTcast(Some thn_typ, Some thn_ast), 
          GTcast(Some el_typ, Some el_ast) when globalType_equal thn_typ el_typ -> 
            GTcast(Some thn_typ, Some (Branch (if_ast, thn_ast, el_ast,Some thn_typ)))
        | _ -> raise (TypeCheckingFailedException "If should contain an expression that resolves to boolean and both branches should evaluate to same type.") 
      )
    | Assoc (loc, Variable(Name name, typ), _) ->
      (match local_type_check (Variable(Name name, typ)) loc typeMap with 
        | LTcast(Some typ1, Some ast1) -> 
              GTcast( Some (DotType( loc, typ1)), Some (Assoc(loc, ast1, Some (DotType(loc, typ1)))))
        | LTcast(None, Some Variable(Name l_name, _)) -> 
          let Location loc_str = loc in
          (match ImmutableMap.find_opt (loc_str^"."^name) typeMap with
            | Some (DotType(l, typ)) -> GTcast(Some (DotType(l, typ)), Some(Assoc (loc, Variable(Name l_name, Some typ), Some (DotType(l, typ)))))
            | Some _ -> raise (TypeCheckingFailedException "Assoc can only have P.t type")
            | None -> raise (TypeCheckingFailedException "Variable type is not declared")
          ) 
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Assoc")
      )
    | Assoc (loc, arg, _) ->
      (match local_type_check arg loc typeMap with 
        | LTcast(Some typ1, Some ast1) -> 
          GTcast(Some (DotType(loc, typ1)), Some (Assoc (loc, ast1, Some (DotType(loc, typ1)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Assoc")
      )
      (* change this *)
    | Snd (Assoc(loc, arg, typ), rcvng_location, _) ->
        (match type_check (Assoc(loc, arg, typ)) typeMap with 
          | GTcast(Some DotType(_, assoc_typ), Some assoc_ast) -> 
              GTcast(Some (DotType(rcvng_location, assoc_typ)), 
                Some (Snd(assoc_ast, rcvng_location, Some (DotType(rcvng_location, assoc_typ)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Snd")
        )
    | Snd (_, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Snd | sndr can only be Assoc")    
    | Let (Location loc, Variable(Name bndr, Some bndr_typ), snd, thn, _) -> 
      let _new_map = ImmutableMap.add (loc ^"."^ bndr) (DotType(Location loc, bndr_typ)) typeMap in
      (match type_check snd _new_map, type_check thn _new_map with
        | GTcast(Some DotType(_, snd_typ), Some snd_ast), GTcast(Some thn_typ, Some thn_ast) 
          when localType_equal snd_typ bndr_typ ->
            GTcast(Some thn_typ, Some (Let (Location loc, Variable(Name bndr, Some bndr_typ), snd_ast, thn_ast, Some thn_typ)))
        | GTcast(_, _), GTcast(_, _) ->
          (* let _ = print_endline (string_of_bool(localType_equal snd_typ bndr_typ)) in  *)
          raise (TypeCheckingFailedException "Variable should be assigned value of same type")
        | _ -> raise (TypeCheckingFailedException "Let error")
      ) 
    | Let (_, _, _, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Let")    
    | Sync (Location sndr, Direction d, Location rcvr, thn, _) ->
      (match type_check thn typeMap with
        | GTcast(Some thn_typ, Some thn_ast) -> 
          GTcast(Some thn_typ, Some (Sync (Location sndr, Direction d, Location rcvr, thn_ast, Some thn_typ)))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Sync")
      )
    | Application (funct, arg, _) ->
      (match type_check funct typeMap, type_check arg typeMap with
        | GTcast(Some (ArrowType(ityp, otyp)), Some fun_ast), GTcast(Some arg_typ, Some arg_ast) 
        when globalType_equal ityp arg_typ ->
          GTcast(Some otyp, Some (Application(fun_ast, arg_ast, Some otyp)))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Application")
      )
    |FunG (name, ChoreoVars(Name bndr_name, Some bndr_typ), body, Some typ) ->
      let _new_map = ImmutableMap.add bndr_name bndr_typ typeMap in
      (match type_check (ChoreoVars(Name bndr_name, Some bndr_typ)) _new_map, type_check body _new_map with
        | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) 
          when globalType_equal (typ) (ArrowType(arg_typ, body_typ))->
          GTcast(Some (ArrowType(arg_typ, body_typ)), Some (FunG (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
        | GTcast(_, _), GTcast(_, _) ->
          raise (TypeCheckingFailedException "FunG type mismatch")
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunG with type defined")
      )
    |FunG (name, ChoreoVars(Name bndr_name, Some bndr_typ), body, None) ->
      let _new_map = ImmutableMap.add bndr_name bndr_typ typeMap in
      (match type_check (ChoreoVars(Name bndr_name, Some bndr_typ)) _new_map, type_check body _new_map with
        | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) ->
          GTcast(Some (ArrowType(arg_typ, body_typ)), Some (FunG (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunG without type declaration")
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
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunL with type defined")
      )
    |FunL (name, Location loc, Variable(Name bndr, Some bndr_typ), body, None) ->
      let _new_map = ImmutableMap.add (loc ^"."^ bndr) (DotType(Location loc, bndr_typ)) typeMap in
      (match type_check body _new_map with
        | GTcast(Some body_typ, Some body_ast) ->
          GTcast(Some (ArrowType(DotType(Location loc, bndr_typ), body_typ)), 
          Some (FunL (name, Location loc, Variable(Name bndr, Some bndr_typ), body_ast, 
          Some (ArrowType(DotType(Location loc, bndr_typ), body_typ)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunL without type defined")
      )
    (* | Calling *)
    | _ -> raise (TypeCheckingFailedException "No Case matched") 

(* 
let _ast = (Expr.Assoc ((Expr.Location "person"),
(Expr.Variable ((Expr.Name "name"), (Some Expr.StringType))), None)) *)

let ast2 = (Expr.Let ((Basictypes.Location "p1"),
(Expr.Variable ((Basictypes.Name "x"), (Some Basictypes.IntType))),
(Expr.Assoc ((Basictypes.Location "p1"), (Expr.INT 5), None)),
(Expr.Branch (
   (Expr.Assoc ((Basictypes.Location "p1"),
      (Expr.Condition ((Expr.Variable ((Basictypes.Name "x"), None)),
         Basictypes.Gt, (Expr.INT 2), (Some Basictypes.BoolType))),
      None)),
   (Expr.Branch (
      (Expr.Assoc ((Basictypes.Location "p1"),
         (Expr.Condition ((Expr.Variable ((Basictypes.Name "x"), None)),
            Basictypes.Gt, (Expr.INT 10), (Some Basictypes.BoolType))),
         None)),
      (Expr.Sync ((Basictypes.Location "p1"), (Basictypes.Direction "L"),
         (Basictypes.Location "p2"),
         (Expr.Let ((Basictypes.Location "p2"),
            (Expr.Variable ((Basictypes.Name "y"),
               (Some Basictypes.IntType))),
            (Expr.Snd (
               (Expr.Assoc ((Basictypes.Location "p1"),
                  (Expr.Plus (
                     (Expr.Variable ((Basictypes.Name "x"), None)),
                     (Expr.INT 2), (Some Basictypes.IntType))),
                  None)),
               (Basictypes.Location "p2"), None)),
            (Expr.Assoc ((Basictypes.Location "p2"),
               (Expr.Variable ((Basictypes.Name "y"), None)), None)),
            None)),
         None)),
      (Expr.Sync ((Basictypes.Location "p1"), (Basictypes.Direction "R"),
         (Basictypes.Location "p2"),
         (Expr.Let ((Basictypes.Location "p2"),
            (Expr.Variable ((Basictypes.Name "y"),
               (Some Basictypes.IntType))),
            (Expr.Snd (
               (Expr.Assoc ((Basictypes.Location "p1"),
                  (Expr.Minus (
                     (Expr.Variable ((Basictypes.Name "x"), None)),
                     (Expr.INT 2), (Some Basictypes.IntType))),
                  None)),
               (Basictypes.Location "p2"), None)),
            (Expr.Assoc ((Basictypes.Location "p2"),
               (Expr.Variable ((Basictypes.Name "y"), None)), None)),
            None)),
         None)),
      None)),
   (Expr.Branch (
      (Expr.Assoc ((Basictypes.Location "p1"),
         (Expr.Condition ((Expr.Variable ((Basictypes.Name "x"), None)),
            Basictypes.Gt, (Expr.INT 10), (Some Basictypes.BoolType))),
         None)),
      (Expr.Sync ((Basictypes.Location "p1"), (Basictypes.Direction "L"),
         (Basictypes.Location "p2"),
         (Expr.Let ((Basictypes.Location "p2"),
            (Expr.Variable ((Basictypes.Name "y"),
               (Some Basictypes.IntType))),
            (Expr.Snd (
               (Expr.Assoc ((Basictypes.Location "p1"),
                  (Expr.Plus (
                     (Expr.Variable ((Basictypes.Name "x"), None)),
                     (Expr.INT 2), (Some Basictypes.IntType))),
                  None)),
               (Basictypes.Location "p2"), None)),
            (Expr.Assoc ((Basictypes.Location "p2"),
               (Expr.Variable ((Basictypes.Name "y"), None)), None)),
            None)),
         None)),
      (Expr.Sync ((Basictypes.Location "p1"), (Basictypes.Direction "R"),
         (Basictypes.Location "p2"),
         (Expr.Let ((Basictypes.Location "p2"),
            (Expr.Variable ((Basictypes.Name "y"),
               (Some Basictypes.IntType))),
            (Expr.Snd (
               (Expr.Assoc ((Basictypes.Location "p1"),
                  (Expr.Minus (
                     (Expr.Variable ((Basictypes.Name "x"), None)),
                     (Expr.INT 2), (Some Basictypes.IntType))),
                  None)),
               (Basictypes.Location "p2"), None)),
            (Expr.Assoc ((Basictypes.Location "p2"),
               (Expr.Variable ((Basictypes.Name "y"), None)), None)),
            None)),
         None)),
      None)),
   None)),
None))



(* let ast = Expr.Plus {lft = (Expr.INT 3); rght = (Expr.INT 5);
typ = (Some typ_int)} *)

let () = 
  let typeMap = ImmutableMap.empty in
  let res = type_check ast2 typeMap in
  let r = match res with
    | GTcast(_, Some ast1) -> show_expr ast1
    | _ -> ""
  in Printf.printf "%s\n\n" r