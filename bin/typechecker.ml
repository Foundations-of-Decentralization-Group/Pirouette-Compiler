open Expr

exception TypeCheckingFailedException of string

type tc_ast = Tcast of { typ : string option; ast: expr option;}


let rec type_check (expr_ast: expr) (typeMap: (string, string) Hashtbl.t): tc_ast =
  match expr_ast with
    | STRING s -> Tcast{typ = Some("string"); ast = Some (STRING s)}
    | INT i -> Tcast{typ = Some ("int"); ast = Some (INT i)}
    | BOOL b -> Tcast{typ = Some ("bool"); ast = Some (BOOL b)}
    | Variable{name; typ} -> Tcast{typ; ast = Some (Variable{name; typ})}
    | Condition {lft; op; rght; _} -> 
      let tc_lft = type_check lft typeMap in
      let tc_rght = type_check rght typeMap in
      (match tc_lft, tc_rght with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ = Some typ2; ast = Some ast2} -> 
          (match typ1, typ2 with
            | "int", "int" -> 
              Tcast{typ = Some ("bool"); ast = Some (Condition {lft = ast1; op; rght = ast2; typ = Some ("bool")})}
            | _ -> raise (TypeCheckingFailedException "Condition expects left and right arguments to be Integer values") 
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Condition") 
      )
    | Plus {lft; rght; _} -> 
      let tc_lft = type_check lft typeMap in
      let tc_rght = type_check rght typeMap in
      (match tc_lft, tc_rght with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2} -> 
          (match typ1, typ2 with
            | "int", "int" -> 
              Tcast{typ = Some ("int"); ast = Some (Plus {lft = ast1; rght = ast2; typ = Some ("int")})}
            | _ -> raise (TypeCheckingFailedException "Plus expects left and right arguments to be Integer values") 
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Plus") 
      )
    | Minus {lft; rght; _} -> 
      let tc_lft = type_check lft typeMap in
      let tc_rght = type_check rght typeMap in
      (match tc_lft, tc_rght with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2} -> 
          (match typ1, typ2 with
            | "int", "int" -> 
              Tcast{typ = Some ("int"); ast = Some (Minus {lft = ast1; rght = ast2; typ = Some ("int")})}
            | _ -> raise (TypeCheckingFailedException "Minus expects left and right arguments to be Integer values") 
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Minus") 
      )
    | Product {lft; rght; _} -> 
      let tc_lft = type_check lft typeMap in
      let tc_rght = type_check rght typeMap in
      (match tc_lft, tc_rght with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2} -> 
          (match typ1, typ2 with
            | "int", "int" -> 
              Tcast{typ = Some ("int"); ast = Some (Product {lft = ast1; rght = ast2; typ = Some ("int")})}
            | _ -> raise (TypeCheckingFailedException "Product expects left and right arguments to be Integer values") 
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Product") 
      )
    | Division {lft; rght; _} -> 
      let tc_lft = type_check lft typeMap in
      let tc_rght = type_check rght typeMap in
      (match tc_lft, tc_rght with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2} -> 
          (match typ1, typ2 with
            | "int", "int" -> 
              Tcast{typ = Some ("int"); ast = Some (Division {lft = ast1; rght = ast2; typ = Some ("int")})}
            | _ -> raise (TypeCheckingFailedException "Division expects left and right arguments to be Integer values") 
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Division") 
      )
    | Branch { ift; thn; el; _ } ->
      let tc_ift = type_check ift typeMap in
      let tc_thn = type_check thn typeMap in
      let tc_el = type_check el typeMap in
      (match tc_ift, tc_thn, tc_el with
        | Tcast{typ = Some typ1; ast = Some ast1}, Tcast{typ =Some typ2; ast = Some ast2}, 
          Tcast{typ = Some typ3; ast = Some ast3} 
          when typ2 = typ3 && typ1 = "bool"-> 
            Tcast{typ = Some (typ2); ast = Some (Branch { ift = ast1; thn = ast2; el = ast3; typ = Some (typ2)})}
        | _ -> raise (TypeCheckingFailedException "If should contain an expression that resolves to boolean and both branches should evaluate to same type.") 
      )
    | Assoc {loc; arg = Variable{name; typ}; _} ->
      let tc_arg = type_check (Variable{name; typ}) typeMap  in
      (match tc_arg with 
        | Tcast{typ = Some typ1; ast = Some ast1} -> 
            let _ = Hashtbl.add typeMap (loc^"."^name) typ1 in 
              Tcast{typ; ast = Some (Assoc {loc; arg = ast1; typ})}
        | Tcast{typ = None; ast = Some ast1} -> 
          (match Hashtbl.find_opt typeMap (loc^"."^name) with
            | Some x -> Tcast{typ = Some x; ast = Some (Assoc {loc; arg = ast1; typ = Some x})}
            (* | None -> raise (TypeCheckingFailedException "Variable type is not declared") *)
            | None -> Tcast{typ = None; ast = Some (Assoc {loc; arg = ast1; typ = None})}
          )
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Assoc {loc; arg; _} ->
      let tc_arg = type_check arg typeMap in
      (match tc_arg with 
        | Tcast{typ = Some typ1; ast = Some ast1} -> 
          Tcast{typ = Some (typ1); ast = Some (Assoc {loc; arg = ast1; typ = Some (typ1)})}
        | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Assoc")
      )
    | Snd {sndr = Assoc{loc; arg; typ}; name = _; typ = _} ->
      let tc_sndr = type_check (Assoc{loc; arg; typ}) typeMap in
        (match tc_sndr with 
          | Tcast{typ = Some typ1; ast = Some ast1} -> 
              Tcast{typ = Some (typ1); ast = Some (Assoc {loc; arg = ast1; typ = Some (typ1)})}
          | _ -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd")
        )
    | Snd {sndr = _; name = _; typ = _} -> raise (TypeCheckingFailedException "Invalid Program exception while evaluating Snd | sndr can only be Assoc")
    | Let {fst; typ = _; snd; thn; _} -> 
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
      )
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


let ast = Expr.Let {
  fst =
  Expr.Assoc {loc = "person";
    arg = Expr.Variable {name = "name"; typ = (Some "string")}; typ = None};
  snd = Expr.Assoc {loc = "person"; arg = (Expr.INT 5); typ = None};
  thn =
  Expr.Let {
    fst =
    Expr.Assoc {loc = "person2";
      arg = Expr.Variable {name = "x"; typ = None}; typ = None};
    snd =
    Expr.Snd {
      sndr =
      Expr.Assoc {loc = "person";
        arg = Expr.Variable {name = "name"; typ = None}; typ = None};
      name = "person2"; typ = None};
    thn =
    Expr.Assoc {loc = "person";
      arg = Expr.Variable {name = "name"; typ = None}; typ = None};
    typ = None};
  typ = None}
(* let ast = Expr.Plus {lft = (Expr.INT 3); rght = (Expr.INT 5);
typ = (Some typ_int)} *)

let () = 
  let typeMap = Hashtbl.create 10 in
  let res = type_check ast typeMap in
  let r = match res with
    | Tcast{typ = _; ast = Some ast1} -> show_expr ast1
    | _ -> ""
  in Printf.printf "%s\n\n" r