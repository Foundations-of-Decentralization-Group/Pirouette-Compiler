open Ctrl
open Expr

module SS = Set.Make(String);;
exception EndpointProjectionFailedException of string

let get_entitities expr : SS.t = 
  let set1 = SS.empty in
    let rec aux acc expr = match expr with
      | Branch (ift, thn, el, _) -> 
        let acc_ift = aux acc ift in
        let acc_thn = aux acc_ift thn in
        let acc_el = aux acc_thn el in
          acc_el
      | Sync (Location sndr, _, Location rcvr, thn, _) ->
        let acc = aux acc thn in
          let acc = SS.union (SS.add sndr acc) (SS.add rcvr acc) in 
            acc
      | Assoc (Location loc, _, _) ->
          (SS.add loc acc)
      | FunL (_, Location loc, _, body, _) ->
        let acc = aux acc body in
          (SS.add loc acc)
      | FunG (_, arg, body, _) ->
        let acc_arg = aux acc arg in
        let acc = aux acc_arg body in
          acc
      | Snd (sndr, Location loc, _) -> 
        let acc_sndr = aux acc sndr in
          (SS.add loc acc_sndr)
      | Let (Location loc, _, snd, thn, _)  ->
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

let ast = (
  Expr.Application {
  funct =
  Expr.Fun {name = "loop"; arg = (Expr.ChoreoVars "X");
    body = Expr.Calling {name = "loop"; arg = (Expr.ChoreoVars "X")}};
  argument = Expr.Assoc {loc = "person1"; arg = (Expr.Value 0)}}
)

let entities : SS.t = 
  get_entitities ast

let rec merge_branch (lbranch:ctrl) (rbranch:ctrl) : ctrl option= 
  match lbranch, rbranch with
    | ChoreoVars x, ChoreoVars y when x = y -> Some (ChoreoVars x)
    | Unit, Unit -> Some Unit
    | Ret {arg = x}, Ret {arg = y} when x = y -> Some (Ret {arg = x})
    | Branch {ift; thn; el}, Branch {ift = ift2; thn = thn2; el = el2} 
      when ift = ift2 ->
        let merged_thn = merge_branch thn thn2 in
        let merged_el = merge_branch el el2 in
        (match merged_thn, merged_el with
            | Some merged_thn, Some merged_el -> Some (Ctrl.Branch {ift; thn = merged_thn; el = merged_el})
            | _ -> None)
    | Snd {arg; loc; thn}, Snd {arg = arg2; loc = loc2; thn = thn2} 
      when arg = arg2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.Snd {arg; loc; thn = merged_thn})
          | _ -> None)
    | Rcv {arg; loc; thn}, Rcv {arg = arg2; loc = loc2; thn = thn2} 
      when arg = arg2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.Rcv {arg; loc; thn = merged_thn})
          | _ -> None)
    | Choose {d; loc; thn}, Choose {d = d2; loc = loc2; thn = thn2} 
      when d = d2 && loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.Choose {d; loc; thn = merged_thn})
          | _ -> None)
    (* LL *)
    | AllowL {loc; thn}, AllowL {loc = loc2; thn = thn2} 
      when loc = loc2 ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowL {loc; thn = merged_thn})
          | _ -> None)
    (* LR *)
    | AllowL {loc; thn}, AllowR {loc = loc2; thn = thn2}
      when loc = loc2 ->
        Some (AllowLR {loc; thnL = thn; thnR = thn2})
    (* LLR *)
    | AllowL {loc; thn}, AllowLR {loc = loc2; thnL; thnR} 
      when loc = loc2->
        let merged_thn = merge_branch thn thnL in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR {loc; thnL = merged_thn; thnR})
          | _ -> None)
    (* RL *)
    | AllowR {loc; thn}, AllowL {loc = loc2; thn = thn2} 
      when loc = loc2 ->
        Some (AllowLR {loc; thnL = thn2; thnR = thn})
    (* RR *)
    | AllowR {loc; thn}, AllowR {loc = loc2; thn = thn2} 
      when loc = loc2->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowR {loc; thn = merged_thn})
          | _ -> None)
    (* RLR *)
    | AllowR {loc; thn}, AllowLR {loc = loc2; thnL; thnR} 
      when loc = loc2 ->
        let merged_thn = merge_branch thn thnR in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR {loc; thnL; thnR = merged_thn})
          | _ -> None)
    (* LRL *)
    | AllowLR {loc; thnL; thnR}, AllowL {loc = loc2; thn} 
      when loc = loc2 ->
        let merged_thn = merge_branch thnL thn in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR {loc; thnL = merged_thn; thnR})
          | _ -> None)
    (* LRR *)
    | AllowLR {loc; thnL; thnR}, AllowR {loc = loc2; thn}
      when loc = loc2 ->
        let merged_thn = merge_branch thnR thn in
        (match merged_thn with
          | Some merged_thn -> Some (Ctrl.AllowLR {loc; thnL; thnR = merged_thn})
          | _ -> None)
    (* LRLR *)
    | AllowLR {loc; thnL; thnR}, AllowLR {loc = loc2; thnL = thnL2; thnR = thnR2} 
      when loc = loc2 ->
        let merged_thn_l = merge_branch thnL thnL2 in
        let merged_thn_r = merge_branch thnR thnR2 in
        (match merged_thn_l, merged_thn_r with
          | Some merged_thn_l, Some merged_thn_r -> Some (Ctrl.AllowLR {loc; thnL = merged_thn_l; thnR = merged_thn_r})
          | _ -> None)
    | Let {binder; arg; thn}, Let {binder = binder2; arg = arg2; thn = thn2}
        when binder = binder2 -> 
        let merged_arg = merge_branch arg arg2 in
        let merged_thn = merge_branch thn thn2 in
        (match merged_arg, merged_thn with
          | Some merged_arg, Some merged_thn -> Some (Ctrl.Let {binder; arg = merged_arg; thn = merged_thn})
          | _ -> None)
    | Fun {name; arg; body}, Fun {name = name2; arg = arg2; body = body2} 
        when name = name2 && arg = arg2 && body = body2 ->
          Some (Fun {name; arg; body})
    | Application {funct; argument}, Application {funct = funct2; argument = argument2} ->
      let merged_funct = merge_branch funct funct2 in
      let merged_argument = merge_branch argument argument2 in
      (match merged_funct, merged_argument with
          | Some merged_funct, Some merged_argument -> Some (Ctrl.Application {funct = merged_funct; argument = merged_argument})
          | _ -> None)
    | _ -> None



let rec get_ctrlLType (typ : localType) : ctrlType =
  match typ with
  | IntType -> Int
  | StringType -> String
  | BoolType -> Bool

let rec get_ctrlGType (typ : globalType) : ctrlType =
  match typ with
  | DotType(_, Some typ) -> get_ctrlLType typ
  | ArrowType(Some ityp, Some otyp) -> CtrlFun (get_ctrlGType ityp, get_ctrlGType otyp) 

let rec local_ep_ast (expr_ast: expr) : l_ctrl option =
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
  | Branch (ift, thn, el, Some typ) -> p.(m<2)
      (match ep_ast ift currentNode, ep_ast thn currentNode, ep_ast el currentNode with
        | Some ift, Some thn, Some el 
            when currentNode = loc -> 
              Some (Ctrl.Branch (ift, thn, el, (get_ctrlGType typ)))
        | Some _, Some parsed_thn, Some parsed_el -> 
            (match merge_branch parsed_thn parsed_el with
            | None -> None
            | Some x -> Some x)
        | _ -> None)
  | Sync (sndr, d, rcvr, thn, Some typ) ->
    if currentNode = sndr && currentNode = rcvr then
      None
    else if currentNode = sndr && currentNode != rcvr then
      (match ep_ast thn currentNode with
        |Some thn -> Some (Choose (d, rcvr, thn, get_ctrlGType typ))
        | _ -> None)
    else if currentNode = rcvr && currentNode != sndr then
      (match ep_ast thn currentNode, d with
        | Some thn, "L"-> Some (AllowL (sndr, thn, get_ctrlGType typ))
        | Some thn, "R"-> Some (AllowR (sndr, thn, get_ctrlGType typ))
        | _ -> None)
    else
      ep_ast thn currentNode

      (* | Let (_, Variable(Name bndr_arg, Some bndr_typ), Snd (Assoc (loc, arg_snd, Some assoc_typ), rcvr, Some snd_typ), thn, Some thn_typ)-> *)

  | Let (_, bndr_arg, Snd (Assoc (loc, arg_snd, Some assoc_typ), rcvr, Some snd_typ), thn, Some thn_typ)->
    let parsed_thn = ep_ast thn currentNode in
    let parsed_bndr_arg = local_ep_ast bndr_arg in
    let parsed_arg_snd = ep_ast (Assoc (loc, arg_snd, Some assoc_typ)) currentNode in
    (match parsed_thn, parsed_bndr_arg, parsed_arg_snd with
      | Some parsed_thn, Some _, Some parsed_arg_snd when name != currentNode && currentNode = loc
        -> Some (Ctrl.Snd (arg = parsed_arg_snd; loc = name; thn = parsed_thn))
      | Some parsed_thn, Some parsed_bndr_arg, Some _ when name = currentNode && currentNode != loc
      -> Some (Ctrl.Rcv {arg = parsed_bndr_arg; loc; thn = parsed_thn})
      | _, _, _ when name = currentNode && name = loc -> None
      | Some parsed_thn, _, _ when name != currentNode && currentNode != loc -> Some parsed_thn
      | _ -> None)
  (* remove the fresh thing -> intead of function put let () = E1 in E2 in ctrl
      and let _ = E1 in E2 for ocaml  *)
  | Let (loc, arg, snd, thn, Some typ) ->
    (match local_ep_ast arg, ep_ast snd currentNode, ep_ast thn currentNode with
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
    let parsed_body = ep_ast body currentNode in
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
     

let () = SS.iter (fun entity -> 
  let res = ep_ast ast entity in
  let str = "____________________________" ^ entity ^ "___________________________________" in 
  print_endline str;
  let r = match res with
    | Some res -> show_ctrl res
    | None -> ""
  in Printf.printf "%s\n\n" r
  ) entities