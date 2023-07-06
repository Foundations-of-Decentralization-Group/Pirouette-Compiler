open Ctrl
open Expr
open Basictypes

(* module SS = Set.Make(String);; *)
module SS = Set.Make(struct
  type t = location
  let compare = compare_location
end)

exception EndpointProjectionFailedException of string

let get_entitities expr : SS.t = 
  let set1 = SS.empty in
    let rec aux acc expr = match expr with
      | Branch (ift, thn, el, _) -> 
        let acc_ift = aux acc ift in
        let acc_thn = aux acc_ift thn in
        let acc_el = aux acc_thn el in
          acc_el
      | Sync (sndr, _, rcvr, thn, _) ->
        let acc = aux acc thn in
          let acc = SS.union (SS.add sndr acc) (SS.add rcvr acc) in 
            acc
      | Assoc (loc, _, _) ->
          (SS.add loc acc)
      | FunL (_, loc, _, body, _) ->
        let acc = aux acc body in
          (SS.add loc acc)
      | FunG (_, arg, body, _) ->
        let acc_arg = aux acc arg in
        let acc = aux acc_arg body in
          acc
      | Snd (sndr, loc, _) -> 
        let acc_sndr = aux acc sndr in
          (SS.add loc acc_sndr)
      | Let (loc, _, snd, thn, _)  ->
        let acc_snd = aux acc snd in
        let acc_thn = aux acc_snd thn in
          (SS.add loc acc_thn)
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

let rec merge_branch (lbranch:ctrl) (rbranch:ctrl) : ctrl option= 
  (match lbranch, rbranch with
    | ChoreoVars (x, x_typ), ChoreoVars (y, y_typ) 
      when x = y && ctrlType_equal x_typ y_typ -> Some (ChoreoVars (x, x_typ))
    | Unit, Unit -> Some Unit
    | Ret (x, x_typ), Ret (y, y_typ) when x = y && ctrlType_equal x_typ y_typ -> Some (Ret (x, x_typ))
    | Branch (ift, thn, el, x_typ), Branch (ift2, thn2, el2, y_typ) 
      when ift = ift2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        let merged_el = merge_branch el el2 in
        (match merged_thn, merged_el with
            | Some merged_thn, Some merged_el -> Some (Ctrl.Branch (ift, merged_thn, merged_el, x_typ))
            | _ -> None)
    | Snd (arg, loc, thn, x_typ), Snd (arg2, loc2, thn2, y_typ) 
      when arg = arg2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.Snd (arg, loc, merged_thn, x_typ))
          | _ -> None)
    | Rcv (arg, loc, thn, x_typ), Rcv (arg2, loc2, thn2, y_typ) 
    when arg = arg2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
      let merged_thn = merge_branch thn thn2 in
      (match merged_thn with
        | Some merged_thn -> Some (Ctrl.Rcv (arg, loc, merged_thn, x_typ))
        | _ -> None)
    | Choose (d, loc, thn, x_typ), Choose (d2, loc2, thn2, y_typ) 
      when d = d2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.Choose (d, loc, merged_thn, x_typ))
          | _ -> None)
    (* LL *)
    | AllowL (loc, thn, x_typ), AllowL (loc2, thn2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowL (loc, merged_thn, x_typ))
          | _ -> None)
    (* LR *)
    | AllowL (loc, thn, x_typ), AllowR (loc2, thn2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        Some (AllowLR (loc, thn, thn2, x_typ))
    (* LLR *)
    | AllowL (loc, thn, l_typ) , AllowLR (loc2, thnL, thnR, lr_typ) 
      when loc = loc2 && ctrlType_equal l_typ lr_typ ->
        let merged_thn = merge_branch thn thnL in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR (loc, merged_thn, thnR, lr_typ))
          | _ -> None)
    (* RL *)
    | AllowR (loc, thn, x_typ), AllowL (loc2, thn2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        Some (AllowLR (loc, thn2, thn, x_typ))
    (* RR *)
    | AllowR (loc, thn, x_typ), AllowR (loc2, thn2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowR (loc, merged_thn, x_typ))
          | _ -> None)
    (* RLR *)
    | AllowR (loc, thn, x_typ), AllowLR (loc2, thnL, thnR, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ  ->
        let merged_thn = merge_branch thn thnR in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR (loc, thnL, merged_thn, y_typ))
          | _ -> None)
    (* LRL *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowL (loc2, thn, y_typ)
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thnL thn in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR (loc, merged_thn, thnR, x_typ))
          | _ -> None)
    (* LRR *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowR (loc2, thn, y_typ)
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thnR thn in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR (loc, thnL, merged_thn, x_typ))
          | _ -> None)
    (* LRLR *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowLR (loc2, thnL2, thnR2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn_l = merge_branch thnL thnL2 in
        let merged_thn_r = merge_branch thnR thnR2 in
        (match merged_thn_l, merged_thn_r with
          | Some merged_thn_l, Some merged_thn_r -> Some (Ctrl.AllowLR (loc, merged_thn_l, merged_thn_r, x_typ))
          | _ -> None)
    | Let (binder, arg, thn, x_typ), Let (binder2, arg2, thn2, y_typ)
        when binder = binder2 && ctrlType_equal x_typ y_typ -> 
        let merged_arg = merge_branch arg arg2 in
        let merged_thn = merge_branch thn thn2 in
        (match merged_arg, merged_thn with
          | Some merged_arg, Some merged_thn -> Some (Ctrl.Let (binder, merged_arg, merged_thn, x_typ))
          | _ -> None)
    | Fun (name, arg, body, x_typ), Fun (name2, arg2, body2, y_typ) 
        when name = name2 && arg = arg2 && body = body2 && ctrlType_equal x_typ y_typ ->
          Some (Fun (name, arg, body, x_typ))
    | Application (funct, argument, x_typ), Application (funct2, argument2, y_typ) 
      when ctrlType_equal x_typ y_typ ->
      let merged_funct = merge_branch funct funct2 in
      let merged_argument = merge_branch argument argument2 in
      (match merged_funct, merged_argument with
          | Some merged_funct, Some merged_argument -> Some (Ctrl.Application (merged_funct, merged_argument, x_typ))
          | _ -> None)
    | _ -> None
  )


let get_ctrlLType (typ : localType) : ctrlType =
  match typ with
  | IntType -> Int
  | StringType -> String
  | BoolType -> Bool

let rec get_ctrlGType (typ : globalType) : ctrlType =
  match typ with
  | DotType(_, typ) -> get_ctrlLType typ
  | ArrowType(ityp, otyp) -> CtrlFun (get_ctrlGType ityp, get_ctrlGType otyp) 

let rec local_ep_ast (expr_ast: l_expr) : l_ctrl option =
  match expr_ast with
  | INT x -> Some (INT x)
  | STRING x -> Some (STRING x)
  | BOOL x -> Some (BOOL x)
  | Variable (name, Some typ) -> Some (Variable (name, (get_ctrlLType typ)))
  | Plus (lft, rght, Some typ) -> 
    (match local_ep_ast lft, local_ep_ast rght with
      | Some lft, Some rght -> 
        Some (Ctrl.Plus (lft, rght, (get_ctrlLType typ)))
      | _ -> None)
  | Minus (lft, rght, Some typ) -> 
    (match local_ep_ast lft, local_ep_ast rght with
      | Some lft, Some rght -> 
        Some (Ctrl.Minus (lft, rght, (get_ctrlLType typ)))
      | _ -> None)
  | Product (lft, rght, Some typ) -> 
    (match local_ep_ast lft, local_ep_ast rght with
      | Some lft, Some rght -> 
        Some (Ctrl.Product (lft, rght, (get_ctrlLType typ)))
      | _ -> None)
  | Division (lft, rght, Some typ) -> 
    (match local_ep_ast lft, local_ep_ast rght with
      | Some lft, Some rght -> 
        Some (Ctrl.Division (lft, rght, (get_ctrlLType typ)))
      | _ -> None)
  | Condition (lft, op, rght, Some typ) -> 
    (match local_ep_ast lft, local_ep_ast rght with
      | Some lft, Some rght -> 
        Some (Ctrl.Condition (lft, op, rght, (get_ctrlLType typ)))
      | _ -> None)
  | _ -> raise (EndpointProjectionFailedException "EPP failed | Type not found")
       

  (* let rec ep_ast expr currentNode = *)
let rec ep_ast (expr_ast: expr) (currentNode: location): ctrl option =
  match expr_ast with
  | Assoc (loc, arg, Some typ) ->
    (match local_ep_ast arg with
        | Some arg when currentNode = loc -> Some (Ret (arg, (get_ctrlGType typ)))
        | Some _ -> Some Unit
        | _ -> None)
  | Branch (Assoc(loc, arg, assoc_typ), thn, el, Some typ) -> 
      (match ep_ast (Assoc(loc, arg, assoc_typ)) currentNode, ep_ast thn currentNode, ep_ast el currentNode with
        | Some ift, Some thn, Some el 
            when currentNode = loc -> 
              Some (Ctrl.Branch (ift, thn, el, (get_ctrlGType typ)))
        | Some _, Some parsed_thn, Some parsed_el -> 
            (match merge_branch parsed_thn parsed_el with
            | None -> None
            | Some x -> Some x)
        | _ -> None)
  | Sync (sndr, Direction d, rcvr, thn, Some typ) ->
    if currentNode = sndr && currentNode = rcvr then
      None
    else if currentNode = sndr && currentNode != rcvr then
      (match ep_ast thn currentNode with
        |Some thn -> Some (Choose (Direction d, rcvr, thn, get_ctrlGType typ))
        | _ -> None)
    else if currentNode = rcvr && currentNode != sndr then
      (match ep_ast thn currentNode, d with
        | Some thn, "L"-> Some (AllowL (sndr, thn, get_ctrlGType typ))
        | Some thn, "R"-> Some (AllowR (sndr, thn, get_ctrlGType typ))
        | _ -> None)
    else
      ep_ast thn currentNode

  | Let (_, bndr_arg, Snd (Assoc (loc, arg_snd, Some _), rcvr, Some _), thn, Some thn_typ)->
    let parsed_thn = ep_ast thn currentNode in
    let parsed_bndr_arg = local_ep_ast bndr_arg in
    let parsed_arg_snd = local_ep_ast arg_snd in
    (match parsed_thn, parsed_bndr_arg, parsed_arg_snd with
      | Some parsed_thn, Some _, Some parsed_arg_snd when rcvr != currentNode && currentNode = loc
        -> Some (Ctrl.Snd (parsed_arg_snd, rcvr, parsed_thn, (get_ctrlGType thn_typ)))
      | Some parsed_thn, Some parsed_bndr_arg, Some _ when rcvr = currentNode && currentNode != loc 
        -> Some (Ctrl.Rcv (parsed_bndr_arg, loc, parsed_thn, (get_ctrlGType thn_typ)))
      | Some parsed_thn, _, _ when rcvr != currentNode && currentNode != loc -> Some parsed_thn
      | _ -> None)

  | Let (loc, Variable(var_name, Some var_typ), snd, thn, Some typ) ->
    (match ep_ast (Assoc(loc, Variable(var_name, Some var_typ), Some (DotType(loc, var_typ)))) currentNode, ep_ast snd currentNode, ep_ast thn currentNode with
      | Some parsed_arg, Some parsed_snd, Some parsed_thn when loc = currentNode ->
        Some (Ctrl.Let (parsed_arg, parsed_snd, parsed_thn, (get_ctrlGType typ)))
      | Some _, Some parsed_snd, Some parsed_thn when loc != currentNode ->
        Some (Ctrl.Let (Unit, parsed_snd, parsed_thn, (get_ctrlGType typ)))
      | _ -> None)
  (* remove the local function implementation*)
  (* | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
    let parsed_body = ep_ast body currentNode in
    let parsed_arg = ep_ast arg2 currentNode in
    (match parsed_body, parsed_arg with 
      | Some parsed_body, Some parsed_arg when loc = currentNode ->
        Some (Ctrl.Fun {name ; arg = parsed_arg; body = parsed_body})
      | Some parsed_body, Some _ when loc != currentNode ->
        Some (Ctrl.Fun {name ; arg = ChoreoVars (get_fresh_cname()); body = parsed_body})
      | _ -> None) *)
  | FunG (name, arg, body, Some typ) -> 
    (match ep_ast arg currentNode, ep_ast body currentNode with
      | Some argument, Some parsed_body -> Some (Ctrl.Fun (name, argument, parsed_body, (get_ctrlGType typ)))
      | _ -> None)
  (* | Calling {name; arg} ->  
    let parsed_arg = ep_ast arg currentNode in
    (match parsed_arg with
     | Some parsed_arg -> Some (Ctrl.Calling {name; arg = parsed_arg})
     | _ -> None) *)
  | Application (funct, argument, Some typ) ->
      (match ep_ast funct currentNode, ep_ast argument currentNode with
        | Some parsed_funct, Some parsed_argument ->
          Some (Ctrl.Application (parsed_funct, parsed_argument, (get_ctrlGType typ)))
        | _ -> None)
  | ChoreoVars (x, Some typ) -> Some (ChoreoVars (x, (get_ctrlGType typ)))
  | Snd _ -> None
  | _ -> raise (EndpointProjectionFailedException "Case not executed")
     

let ast = (Expr.Application (                 
  (Expr.FunG ((Basictypes.Name "fname"),
     (Expr.ChoreoVars ((Basictypes.Name "X"),
        (Some (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType)))
        )),
     (Expr.Let ((Basictypes.Location "p"),
        (Expr.Variable ((Basictypes.Name "n"), (Some Basictypes.IntType))),
        (Expr.ChoreoVars ((Basictypes.Name "X"),
           (Some (Basictypes.DotType ((Basictypes.Location "p"),
                    Basictypes.IntType)))
           )),
        (Expr.Assoc ((Basictypes.Location "p"),
           (Expr.Plus (
              (Expr.Variable ((Basictypes.Name "n"),
                 (Some Basictypes.IntType))),
              (Expr.INT 1), (Some Basictypes.IntType))),
           (Some (Basictypes.DotType ((Basictypes.Location "p"),
                    Basictypes.IntType)))
           )),
        (Some (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType)))
        )),
     (Some (Basictypes.ArrowType (
              (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType)),
              (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType))
              )))
     )),
  (Expr.Let ((Basictypes.Location "p"),
     (Expr.Variable ((Basictypes.Name "m"), (Some Basictypes.IntType))),
     (Expr.Assoc ((Basictypes.Location "p"), (Expr.INT 3),
        (Some (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType)))
        )),
     (Expr.Assoc ((Basictypes.Location "p"),
        (Expr.Minus (
           (Expr.Variable ((Basictypes.Name "m"), (Some Basictypes.IntType)
              )),
           (Expr.INT 1), (Some Basictypes.IntType))),
        (Some (Basictypes.DotType ((Basictypes.Location "p"),
                 Basictypes.IntType)))
        )),
     (Some (Basictypes.DotType ((Basictypes.Location "p"),
              Basictypes.IntType)))
     )),
  (Some (Basictypes.DotType ((Basictypes.Location "p"), Basictypes.IntType)))
  ))

let entities : SS.t = get_entitities ast

let () = SS.iter (fun entity -> 
  let res = ep_ast ast entity in
  let Location str_entity = entity in
  let str = "____________________________" ^ str_entity ^ "___________________________________" in 
  print_endline str;
  let r = match res with
    | Some res -> show_ctrl res
    | None -> ""
  in Printf.printf "%s\n\n" r
  ) entities