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
  (* | Map (name, var, _) -> prices[b]
    (match local_type_check name, local_type_check var with
      | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
        (match typ1, typ2 with
          | StringType, IntType -> 
            LTcast(Some IntType, Some (Map (ast1, ast2, Some IntType)))
          | StringType, StringType -> 
            LTcast(Some StringType, Some (Map (ast1, ast2, Some StringType)))
          | _ -> raise (TypeCheckingFailedException "Map expects name to be string and argument to be Integer values") 
        )
      | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Map")
      ) *)
  | _ -> raise (TypeCheckingFailedException "Not executed")

let rec type_check (expr_ast: expr) (typeMap: globalType ImmutableMap.t): tc_ast =
  match expr_ast with
    | ChoreoVars (Name name, typ) -> GTcast(typ, Some (ChoreoVars (Name name, typ)))
    (* | Branch { ift; thn; el; _ } ->
      let tc_ift = type_check ift typeMap in
      let tc_thn = type_check thn typeMap in
      let tc_el = type_check el typeMap in
      (match tc_ift, tc_thn, tc_el with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2}, 
          Tcast{typ = Some typ3; ast = Some ast3} 
          when typ2 = typ3 && typ1 = "bool"-> 
            Tcast{typ = Some (typ2); ast = Some (Branch { ift = ast1; thn = ast2; el = ast3; typ = Some (typ2)})}
        | _ -> raise (TypeCheckingFailedException "If should contain an expression that resolves to boolean and both branches should evaluate to same type.") 
      ) *) 
    | Assoc (loc, Variable(Name name, typ), _) ->
      (* let tc_arg = type_check (Variable{name; typ}) typeMap  in *)
      (match local_type_check (Variable(Name name, typ)) with 
        | LTcast(Some typ1, Some ast1) -> 
          let Location location = loc in
          let _new_map = ImmutableMap.add (location ^"."^ name) (DotType(loc, typ1)) typeMap in
            (* let _ = Hashtbl.add typeMap (loc^"."^name) typ1 in  *)
              GTcast( Some (DotType( loc, typ1)), Some (Assoc(loc, ast1, Some (DotType(loc, typ1)))))
        (* | LTcast{typ = None; ast = Some ast1} -> 
          (match Hashtbl.find_opt typeMap (loc^"."^name) with
            | Some x -> Tcast{typ = Some x; ast = Some (Assoc {loc; arg = ast1; typ = Some x})}
            (* | None -> raise (TypeCheckingFailedException "Variable type is not declared") *)
            | None -> Tcast{typ = None; ast = Some (Assoc {loc; arg = ast1; typ = None})}
          ) *)
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Assoc (loc, arg, _) ->
      (match local_type_check arg with 
        | LTcast(Some typ1, Some ast1) -> 
          GTcast(Some (DotType(loc, typ1)), Some (Assoc (loc, ast1, Some (DotType(loc, typ1)))))
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Snd (Assoc(loc, arg, typ), name, _) ->
        (match type_check (Assoc(loc, arg, typ)) typeMap with 
          | GTcast(Some typ1, Some Assoc(loc, arg, Some typ2)) -> 
              GTcast(Some typ1, Some (Snd((Assoc (loc, arg, Some typ2)), name, Some typ1)))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd")
        )
    | Snd (_, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd | sndr can only be Assoc")
(*     
    | Let (fst, snd, thn, _) -> 
      (match type_check snd typeMap, type_check fst typeMap, type_check thn typeMap with
        | GTcast(Some typ1, Some ast1), GTcast(Some typ2, Some ast2), GTcast(Some typ3, Some ast3)
          when typ1 = typ2 ->
            GTcast(Some typ3, Some (Let (ast1, ast2, ast3, Some typ3)))
        | GTcast(None, Some (Assoc(loc, arg, None))), GTcast(Some typ2, Some ast2), 
          GTcast(Some typ3, Some ast3) ->
            GTcast(typ = Some typ3, Some (Let (Assoc(loc, arg, Some typ2), ast2, ast3, Some typ3)))
        | _ -> raise (TypeCheckingFailedException "Variable should be assigned value of same type")
      ) *)
    (* | Let {fst; snd; thn; _} -> 
      let tc_snd = type_check snd typeMap in
      let tc_fst = type_check fst typeMap in
      let tc_thn = type_check thn typeMap in
      (match tc_fst, tc_snd, tc_thn with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2}, 
          Tcast{typ = Some typ3; ast = Some ast3} 
          when typ1 = typ2 ->
            Tcast{typ = Some (typ3); ast = Some (Let {fst = ast1; snd = ast2; thn = ast3; typ = Some (typ3)})}
        | Tcast{typ = None; ast = Some (Assoc{loc; arg; typ = None})}, Tcast{typ =Some typ2; ast = Some ast2}, 
          Tcast{typ = Some typ3; ast = Some ast3} ->
            Tcast{typ = Some (typ3); ast = Some (Let {fst = Assoc{loc; arg; typ = Some (typ2)}; snd = ast2; thn = ast3; typ = Some (typ3)})}
        | _ -> raise (TypeCheckingFailedException "Variable should be assigned value of same type")
      ) *)
    | _ -> raise (TypeCheckingFailedException "No Case matched") 


let _ast = (Expr.Assoc ((Expr.Location "person"),
(Expr.Variable ((Expr.Name "name"), (Some Expr.StringType))), None))

let ast2 = (Expr.Assoc ((Expr.Location "person"),
(Expr.Plus ((Expr.INT 2), (Expr.INT 3), (Some Expr.IntType))), None))
(* let ast = Expr.Plus {lft = (Expr.INT 3); rght = (Expr.INT 5);
typ = (Some typ_int)} *)

let () = 
  let typeMap = ImmutableMap.empty in
  let res = type_check ast2 typeMap in
  let r = match res with
    | GTcast(_, Some ast1) -> show_expr ast1
    | _ -> ""
  in Printf.printf "%s\n\n" r