open Basictypes


module LocalExpr = struct
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

  exception LocalTypeCheckingFailedException of string
  
  type tc_ast = 
    | LTcast of localType option * l_expr option

  let rec local_type_check (l_expr_ast: l_expr) (typeMap: localType LocalMap.t) = 
    match l_expr_ast with
    | STRING s -> LTcast(Some StringType, Some (STRING s))
    | INT i -> LTcast(Some IntType, Some (INT i))
    | BOOL b -> LTcast(Some BoolType, Some (BOOL b))
    | Variable(Name name, typ) -> 
      (match ImmutableMap.find_opt name typeMap, typ with
        | Some inf_typ, Some dec_typ 
          when localType_equal inf_typ dec_typ ->
          LTcast(Some dec_typ, Some (Variable (Name name, Some dec_typ)))
        | Some _, Some _ -> 
          raise (LocalTypeCheckingFailedException "inferred type and declared type do not match")
        | Some inf_typ, None ->
          LTcast(Some inf_typ, Some (Variable (Name name, Some inf_typ)))
        | None, Some dec_typ->
          LTcast(Some dec_typ, Some (Variable (Name name, Some dec_typ)))
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Variable")
      )

    | Condition (lft, op, rght, _) -> 
      (match local_type_check lft typeMap, local_type_check rght typeMap with
        | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
          (match typ1, typ2 with
            | IntType, IntType -> 
              LTcast(Some BoolType, Some (Condition (ast1, op, ast2, Some BoolType)))
            | _ -> raise (LocalTypeCheckingFailedException "Condition expects left and right arguments to be Integer values") 
          )
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Condition") 
      )
    | Plus (lft, rght, _) -> 
      (match local_type_check lft typeMap, local_type_check rght typeMap with
        | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
          (match typ1, typ2 with
            | IntType, IntType -> 
              LTcast(Some IntType, Some (Plus (ast1, ast2, Some IntType)))
            | _ -> raise (LocalTypeCheckingFailedException "Plus expects left and right arguments to be Integer values") 
          )
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Plus")
        )
    | Minus (lft, rght, _) -> 
      (match local_type_check lft typeMap, local_type_check rght typeMap with
        | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
          (match typ1, typ2 with
            | IntType, IntType -> 
              LTcast(Some IntType, Some (Minus (ast1, ast2, Some IntType)))
            | _ -> raise (LocalTypeCheckingFailedException "Minus expects left and right arguments to be Integer values") 
          )
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Minus")
        )
    | Division (lft, rght, _) -> 
      (match local_type_check lft typeMap, local_type_check rght typeMap with
        | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
          (match typ1, typ2 with
            | IntType, IntType -> 
              LTcast(Some IntType, Some (Division (ast1, ast2, Some IntType)))
            | _ -> raise (LocalTypeCheckingFailedException "Division expects left and right arguments to be Integer values") 
          )
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Division")
        )
    | Product (lft, rght, _) -> 
      (match local_type_check lft typeMap, local_type_check rght typeMap with
        | LTcast(Some typ1, Some ast1), LTcast(Some typ2, Some ast2) -> 
          (match typ1, typ2 with
            | IntType, IntType -> 
              LTcast(Some IntType, Some (Product (ast1, ast2, Some IntType)))
            | _ -> raise (LocalTypeCheckingFailedException "Product expects left and right arguments to be Integer values") 
          )
        | _ -> raise (LocalTypeCheckingFailedException "Invalid Program exception while Type checking Product")
      )
end;;



module Expr = struct
  (* *************Imports************ *)
  open LocalExpr

  (* *************Modules************ *)
  module LocationSet = Set.Make(struct
    type t = location
    let compare = compare_location
  end)

  (* *************Exceptions************ *)
  exception TypeCheckingFailedException of string
  
  (* *************Types************ *)
  type expr =
    | ChoreoVars of name * globalType option
    | Branch of expr * expr * expr * globalType option
    | Sync of location * direction * location * expr * globalType option
    | Fun of name * expr * expr * globalType option
    (* | FunL of name * location * l_expr * expr * globalType option *)
    | Calling of name * expr * globalType option
    | Snd of expr * location * globalType option
    | Let of location * l_expr * expr * expr * globalType option
    | Assoc of location * l_expr * globalType option
    | Application of expr * expr * globalType option
  [@@deriving show]

  type tc_ast = 
    | GTcast of globalType option * expr option

  (* *************Methods************ *)
  let rec type_check (expr_ast: expr) (typeMap: localType LocalMap.t ImmutableMap.t) (choreoMap: globalType ChoreoMap.t): tc_ast =
    match expr_ast with
      | ChoreoVars (Name name, typ) -> 
        (match ChoreoMap.find_opt (name) choreoMap, typ with
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
        (match type_check ift typeMap choreoMap, type_check thn typeMap choreoMap, type_check el typeMap choreoMap with
          | GTcast(Some DotType(_, BoolType), Some if_ast), GTcast(Some thn_typ, Some thn_ast), 
            GTcast(Some el_typ, Some el_ast) when globalType_equal thn_typ el_typ -> 
              GTcast(Some thn_typ, Some (Branch (if_ast, thn_ast, el_ast,Some thn_typ)))
          | _ -> raise (TypeCheckingFailedException "If should contain an expression that resolves to boolean and both branches should evaluate to same type.") 
        )
      | Assoc (loc, Variable(Name name, typ), _) ->
        (match local_type_check (Variable(Name name, typ)) (get_local_map typeMap loc) with 
          | LTcast(Some typ1, Some ast1) -> 
                GTcast( Some (DotType( loc, typ1)), Some (Assoc(loc, ast1, Some (DotType(loc, typ1)))))
          | LTcast(None, Some Variable(Name l_name, _)) -> 
            (match find_map typeMap loc name with
              | Some (DotType(l, typ)) -> GTcast(Some (DotType(l, typ)), Some(Assoc (loc, Variable(Name l_name, Some typ), Some (DotType(l, typ)))))
              | Some _ -> raise (TypeCheckingFailedException "Assoc can only have P.t type")
              | None -> raise (TypeCheckingFailedException "Variable type is not declared")
            ) 
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Assoc")
        )
      | Assoc (loc, arg, _) ->
        (match local_type_check arg (get_local_map typeMap loc) with 
          | LTcast(Some typ1, Some ast1) -> 
            GTcast(Some (DotType(loc, typ1)), Some (Assoc (loc, ast1, Some (DotType(loc, typ1)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Assoc")
        )
        (* change this *)
      | Snd (Assoc(loc, arg, typ), rcvng_location, _) ->
          (match type_check (Assoc(loc, arg, typ)) typeMap choreoMap with 
            | GTcast(Some DotType(_, assoc_typ), Some assoc_ast) -> 
                GTcast(Some (DotType(rcvng_location, assoc_typ)), 
                  Some (Snd(assoc_ast, rcvng_location, Some (DotType(rcvng_location, assoc_typ)))))
            | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Snd")
          )
                (* GTcast(Some (DotType(rcvng_location, assoc_typ)),  *)
      | Snd (ChoreoVars(Name x, typ), rcvng_location, _) ->
        (match type_check (ChoreoVars(Name x, typ)) typeMap choreoMap with
          | GTcast(Some DotType(_, typ), Some ast) -> 
            GTcast(Some (DotType(rcvng_location, typ)), 
              Some (Snd(ast, rcvng_location, Some (DotType(rcvng_location, typ)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Snd with choreovar as sender")
         )
      | Snd (_, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Snd | sndr can only be Assoc")    
      | Let (loc, Variable(Name bndr, Some bndr_typ), snd, thn, _) -> 
        let _new_map = add_map typeMap loc bndr bndr_typ in
        (* let _new_map = ImmutableMap.add (loc ^"."^ bndr) (DotType(Location loc, bndr_typ)) typeMap in *)
        (match type_check snd _new_map choreoMap, type_check thn _new_map choreoMap with
          | GTcast(Some DotType(_, snd_typ), Some snd_ast), GTcast(Some thn_typ, Some thn_ast) 
            when localType_equal snd_typ bndr_typ ->
              GTcast(Some thn_typ, Some (Let (loc, Variable(Name bndr, Some bndr_typ), snd_ast, thn_ast, Some thn_typ)))
          | GTcast(_, _), GTcast(_, _) ->
            (* let _ = print_endline (string_of_bool(localType_equal snd_typ bndr_typ)) in  *)
            raise (TypeCheckingFailedException "Variable should be assigned value of same type")
          (* | _ -> raise (TypeCheckingFailedException "Let error") *)
        ) 
      | Let (_, _, _, _, _) -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Let")    
      | Sync (Location sndr, Direction d, Location rcvr, thn, _) ->
        (match type_check thn typeMap choreoMap with
          | GTcast(Some thn_typ, Some thn_ast) -> 
            GTcast(Some thn_typ, Some (Sync (Location sndr, Direction d, Location rcvr, thn_ast, Some thn_typ)))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Sync")
        )
      | Application (funct, arg, _) ->
        (match type_check funct typeMap choreoMap, type_check arg typeMap choreoMap with
          | GTcast(Some (ArrowType(ityp, otyp)), Some fun_ast), GTcast(Some arg_typ, Some arg_ast) 
          when globalType_equal ityp arg_typ ->
            GTcast(Some otyp, Some (Application(fun_ast, arg_ast, Some otyp)))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Application")
        )
      |Fun (name, ChoreoVars(Name bndr_name, Some bndr_typ), body, Some typ) ->
        let _new_map = ChoreoMap.add bndr_name bndr_typ choreoMap in
        (match type_check (ChoreoVars(Name bndr_name, Some bndr_typ)) typeMap _new_map, type_check body typeMap _new_map with
          | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) 
            when globalType_equal (typ) (ArrowType(arg_typ, body_typ))->
            GTcast(Some (ArrowType(arg_typ, body_typ)), Some (Fun (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
          | GTcast(_, _), GTcast(_, _) ->
            raise (TypeCheckingFailedException "Fun type mismatch")
          (* | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Fun with type defined") *)
        )
      |Fun (name, ChoreoVars(Name bndr_name, Some bndr_typ), body, None) ->
        let _new_map = ChoreoMap.add bndr_name bndr_typ choreoMap in
        (match type_check (ChoreoVars(Name bndr_name, Some bndr_typ)) typeMap _new_map, type_check body typeMap _new_map with
          | GTcast(Some arg_typ, Some arg_ast), GTcast(Some body_typ, Some body_ast) ->
            GTcast(Some (ArrowType(arg_typ, body_typ)), Some (Fun (name, arg_ast, body_ast, Some (ArrowType(arg_typ, body_typ)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking Fun without type declaration")
        )
      (* |FunL (name, loc, Variable(Name bndr, Some bndr_typ), body, Some typ) ->
        let _new_map = add_map typeMap loc bndr bndr_typ in
        (match type_check body _new_map choreoMap with
          | GTcast(Some body_typ, Some body_ast) 
            when globalType_equal (typ) (ArrowType(DotType(loc, bndr_typ), body_typ))->
            GTcast(Some (ArrowType(DotType(loc, bndr_typ), body_typ)), 
            Some (FunL (name, loc, Variable(Name bndr, Some bndr_typ), body_ast, 
            Some (ArrowType(DotType(loc, bndr_typ), body_typ)))))
          | GTcast(_, _) ->
            raise (TypeCheckingFailedException "FunL type mismatch")
          (* | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunL with type defined") *)
        )
      |FunL (name, loc, Variable(Name bndr, Some bndr_typ), body, None) ->
        let _new_map = add_map typeMap loc bndr bndr_typ in
        (match type_check body _new_map choreoMap with
          | GTcast(Some body_typ, Some body_ast) ->
            GTcast(Some (ArrowType(DotType(loc, bndr_typ), body_typ)), 
            Some (FunL (name, loc, Variable(Name bndr, Some bndr_typ), body_ast, 
            Some (ArrowType(DotType(loc, bndr_typ), body_typ)))))
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while Type checking FunL without type defined")
        ) *)
      (* | Calling *)
      | _ -> raise (TypeCheckingFailedException "No Case matched") 
  
  let get_entitities expr : LocationSet.t = 
    let set1 = LocationSet.empty in
      let rec aux acc expr = match expr with
        | Branch (ift, thn, el, _) -> 
          let acc_ift = aux acc ift in
          let acc_thn = aux acc_ift thn in
          let acc_el = aux acc_thn el in
            acc_el
        | Sync (sndr, _, rcvr, thn, _) ->
          let acc = aux acc thn in
            let acc = LocationSet.union (LocationSet.add sndr acc) (LocationSet.add rcvr acc) in 
              acc
        | Assoc (loc, _, _) ->
            (LocationSet.add loc acc)
        (* | FunL (_, loc, _, body, _) ->
          let acc = aux acc body in
            (LocationSet.add loc acc) *)
        | Fun (_, arg, body, _) ->
          let acc_arg = aux acc arg in
          let acc = aux acc_arg body in
            acc
        | Snd (sndr, loc, _) -> 
          let acc_sndr = aux acc sndr in
            (LocationSet.add loc acc_sndr)
        | Let (loc, _, snd, thn, _)  ->
          let acc_snd = aux acc snd in
          let acc_thn = aux acc_snd thn in
            (LocationSet.add loc acc_thn)
        | Application (funct, argument, _) -> 
          let acc_funct = aux acc funct in
          let acc = aux acc_funct argument in
            acc
        | Calling (_, arg, _) ->
          let acc_arg = aux acc arg in
          acc_arg 
        | ChoreoVars _ -> acc
      in
        aux set1 expr
end;;
(* module Expr : sig
  include LocalExpr
end *)

