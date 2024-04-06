open Format

(* decl_block {
     stmt1
     stmt2
     stmt3
     ...
   }
*)
let rec print_decl_block ppf (stmts : Choreo.decl_block) =
  fprintf ppf "@[<v 2>DeclBlock {@,%a@,}@]"
    (pp_print_list ~pp_sep:pp_print_newline print_stmt) stmts

(* Decl (choreo_pattern) : choreo_type
   Assign (choreo_pattern) = (choreo_expr)
   TypeDecl id : choreo_type
*)
and print_stmt ppf = function
  | Decl (p, t) ->
      fprintf ppf "@[<hv2>Decl@ (@[<hv2>%a@]) :@ @[<hv2>%a@]@]"
        print_choreo_pattern p
        print_choreo_type t
  | Assign (p, e) ->
      fprintf ppf "@[<hv2>Assign@ (@[<hv2>%a@]) =@ (@[<hv2>%a@])@]"
        print_choreo_pattern p
        print_choreo_expr e
  | TypeDecl (VarId id, t) ->
      fprintf ppf "@[<hv2>TypeDecl@ %s :@ @[<hv2>%a@]@];"
        id
        print_choreo_type t

(* Unit
   Var id
   Fst (expr)
   Snd (expr)
   Left (expr)
   Right (expr)
   LocExpr loc.(local_expr)
   Send (expr) -> loc
   Sync loc1[label] ~> loc2; (expr)
   If (condition) then (expr1) else (expr2)
   Let {decl_block} in (expr)
   FunDef id -> (expr)
   FunApp ((fun) (arg))
   Pair ((expr1), (expr2))
   Match (expr) with
    | case1
    | case2
    | ...
*)
and print_choreo_expr ppf = function
  | Unit -> fprintf ppf "Unit"
  | Var (VarId id) ->
      fprintf ppf "Var %s" id
  | Fst e ->
      fprintf ppf "Fst@ (@[<hv2>%a@])" print_choreo_expr e
  | Snd e ->
      fprintf ppf "Snd@ (@[<hv2>%a@])" print_choreo_expr e
  | Left e ->
      fprintf ppf "Left@ (@[<hv2>%a@])" print_choreo_expr e
  | Right e ->
      fprintf ppf "Right@ (@[<hv2>%a@])" print_choreo_expr e
  | LocExpr (LocId loc, e) ->
      fprintf ppf "LocExpr %s.(@[<hv2>%a@])" loc print_local_expr e
  | Send (e, LocId loc) ->
      fprintf ppf "Send@ (@[<hv2>%a@]) ->@ %s" print_choreo_expr e loc
  | Sync (LocId loc1, LabelId label, LocId loc2, e) ->
      fprintf ppf "Sync@ %s[%s] ~>@ %s;@ (@[<hv2>%a@])" loc1 label loc2 print_choreo_expr e
  | If (e1, e2, e3) ->
      fprintf ppf "If@ (@[<hv2>%a@]) then@ (@[<hv2>%a@]) else@ (@[<hv2>%a@])" print_choreo_expr e1 print_choreo_expr e2 print_choreo_expr e3
  | Let (decl_block, e) ->
      fprintf ppf "Let@ @[<v2>{@,%a@,}@] in (@[<hv2>%a@])" print_decl_block decl_block print_choreo_expr e
  | FunDef (VarId id, e) ->
      fprintf ppf "FunDef %s ->@ (@[<hv2>%a@])" id print_choreo_expr e
  | FunApp (e1, e2) ->
      fprintf ppf "FunApp@ (@[<hv2>%a@])@ (@[<hv2>%a@])" print_choreo_expr e1 print_choreo_expr e2
  | Pair (e1, e2) ->
      fprintf ppf "Pair@ ((@[<hv2>%a@]),@ (@[<hv2>%a@])" print_choreo_expr e1 print_choreo_expr e2
  | Match (e, cases) ->
      fprintf ppf "Match@ (@[<hv2>%a@]) with@ @[<v2>@,%a@]" print_choreo_expr e (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "\n|") print_choreo_case) cases
