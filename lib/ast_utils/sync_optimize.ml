module Local = Ast_core.Local.M
module Choreo = Ast_core.Choreo.M
module Net = Ast_core.Net.M

let rec collect_data
          (p : 'a Choreo.stmt list)
          (holder1 : string list)
          (_holder2 : string list)
  : string list
  =
  match p with
  | [] -> holder1
  | head :: tail ->
    let list_two =
      let list_one = copy_matcher_stmt head holder1 _holder2 in
      collect_data tail list_one _holder2
    in
    list_two

and copy_matcher_stmt
      (x : 'a Choreo.stmt)
      (holder1 : string list)
      (_holder2 : string list)
  : string list
  =
  match x with
  | Assign (_, e, _) ->
    (* this is where we go to the choreo expr variant*)
    let rs_list = visit_choreo_expr e holder1 _holder2 in
    rs_list
  | _ ->
    print_endline "Hit this";
    []

and visit_choreo_expr
      (x : 'a Choreo.expr)
      (holder1 : string list)
      (_holder2 : string list)
  : string list
  =
  match x with
  | Unit _ -> holder1
  | Var (_, _) -> holder1
  | LocExpr (_, _, _) -> holder1
  | Send (_, e, _, _) -> visit_choreo_expr e holder1 _holder2
  | If (e1, e2, _, _) ->
    (* let list_three = *)
    (*   let list_two = *)
    (*     let list_one = visit_choreo_expr e1 holder1 _holder2 in *)
    (*     visit_choreo_expr e2 list_one _holder2 *)
    (*   in *)
    (*   visit_choreo_expr e3 list_two _holder2 *)
    (* in *)
    (* (\* let size = List.length list_three in *\) *)
    (* Printf.printf "This is the size of the internal list %d \n" size; *)
    (* list_three *)
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Let (stmts, e, _) ->
    let list_one = collect_data stmts holder1 _holder2 in
    visit_choreo_expr e list_one _holder2
  | FunDef (_, e, _) -> visit_choreo_expr e holder1 _holder2
  | FunApp (e1, e2, _) ->
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Pair (e1, e2, _) ->
    let list_one = visit_choreo_expr e1 holder1 _holder2 in
    visit_choreo_expr e2 list_one _holder2
  | Fst (e, _) -> visit_choreo_expr e holder1 _holder2
  | Snd (e, _) -> visit_choreo_expr e holder1 _holder2
  | Left (e, _) -> visit_choreo_expr e holder1 _holder2
  | Right (e, _) -> visit_choreo_expr e holder1 _holder2
  | Match (e, _, _) -> visit_choreo_expr e holder1 _holder2
  | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
    print_endline "Hit the Sync Ast node";
    let holder1 = List.cons id1 holder1 in
    let holder1 = List.cons id3 holder1 in
    let rec check_consecutive_syncs (e : 'a Choreo.expr) holder1 =
      match e with
      | Sync (LocId (id1, _), _, LocId (id3, _), e, _) ->
        let holder1 = List.cons id1 holder1 in
        let holder1 = List.cons id3 holder1 in
        check_consecutive_syncs e holder1
      | _ -> holder1
    in
    let list_to_be_reversed = check_consecutive_syncs e holder1 in
    let final_list = List.rev list_to_be_reversed in
    let rec check_optimization_possible input_list output_list switch =
      match input_list with
      | [] -> []
      | head :: tail ->
        if switch mod 2 = 0
        then head :: check_optimization_possible tail output_list (switch + 1)
        else check_optimization_possible tail output_list (switch + 1)
    in
    if List.length final_list = 0
    then (
      print_endline
        "Cannot be optimized because there are no sync statements/control messages";
      [])
    else (
      let result_list = check_optimization_possible final_list [] 0 in
      let rec check_equality input_list acc =
        match input_list with
        | [] -> true
        | head :: tail -> if String.equal head acc then check_equality tail acc else false
      in
      let enable_opt = check_equality (List.tl result_list) (List.hd result_list) in
      let final_list = List.cons (Bool.to_string enable_opt) final_list in
      if enable_opt
       then print_endline "Optimization possible"
       else
         print_endline
           "Optimization not possible because the sender for each of the control \
            messages is different";
        final_list)
;;
