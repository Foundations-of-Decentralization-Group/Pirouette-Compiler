(* *************Imports************ *)
open Basictypes
open Pirouette.LocalExpr
open Pirouette.Expr

(* *************Types************ *)
type ctrlType = 
  | Int
  | String
  | Bool
  | CtrlFun of ctrlType * ctrlType
[@@deriving show]

(* *************Exceptions************ *)
exception EndpointProjectionFailedException of string

module LocalCtrl = struct

  (* *************Types************ *)
  type l_ctrl = 
    | INT of int
    | STRING of string
    | BOOL of bool
    | Variable of name * ctrlType
    | Condition of l_ctrl * binop * l_ctrl * ctrlType
    | Plus of l_ctrl * l_ctrl * ctrlType
    | Minus of l_ctrl * l_ctrl * ctrlType
    | Product of l_ctrl * l_ctrl * ctrlType
    | Division of l_ctrl * l_ctrl * ctrlType
  [@@deriving show]

  (* *************Methods************ *)
  let get_ctrlLType (typ : localType) : ctrlType =
    match typ with
    | IntType -> Int
    | StringType -> String
    | BoolType -> Bool

  let get_ctrlTypeToLCtrlType (typ : ctrlType) : string =
    match typ with
    | Int -> "int"
    | String -> "string"
    | Bool -> "bool"
    | _ -> ""
  
  let getLCtrlType = function
  | STRING _ -> "string"
  | BOOL _ -> "bool"
  | Variable (_, typ) -> get_ctrlTypeToLCtrlType typ
  | Condition (_, _, _, typ) -> get_ctrlTypeToLCtrlType typ
  | _ -> "int" 

  let rec lexpr_to_lctrl (expr_ast: l_expr) : l_ctrl option =
    match expr_ast with
    | INT x -> Some (INT x)
    | STRING x -> Some (STRING x)
    | BOOL x -> Some (BOOL x)
    | Variable (name, Some typ) -> Some (Variable (name, (get_ctrlLType typ)))
    | Plus (lft, rght, Some typ) -> 
      (match lexpr_to_lctrl lft, lexpr_to_lctrl rght with
        | Some lft, Some rght -> 
          Some (Plus (lft, rght, (get_ctrlLType typ)))
        | _ -> None)
    | Minus (lft, rght, Some typ) -> 
      (match lexpr_to_lctrl lft, lexpr_to_lctrl rght with
        | Some lft, Some rght -> 
          Some (Minus (lft, rght, (get_ctrlLType typ)))
        | _ -> None)
    | Product (lft, rght, Some typ) -> 
      (match lexpr_to_lctrl lft, lexpr_to_lctrl rght with
        | Some lft, Some rght -> 
          Some (Product (lft, rght, (get_ctrlLType typ)))
        | _ -> None)
    | Division (lft, rght, Some typ) -> 
      (match lexpr_to_lctrl lft, lexpr_to_lctrl rght with
        | Some lft, Some rght -> 
          Some (Division (lft, rght, (get_ctrlLType typ)))
        | _ -> None)
    | Condition (lft, op, rght, Some typ) -> 
      (match lexpr_to_lctrl lft, lexpr_to_lctrl rght with
        | Some lft, Some rght -> 
          Some (Condition (lft, op, rght, (get_ctrlLType typ)))
        | _ -> None)
    | _ -> raise (EndpointProjectionFailedException "EPP failed | Type not found")
  
end


module Ctrl = struct
  (* *************Imports************ *)
  open LocalCtrl

  type ctrl =
    | Unit
    | Ret of l_ctrl * ctrlType
    | ChoreoVars of name * ctrlType
    | Snd of l_ctrl * location * ctrl * ctrlType
    | Rcv of l_ctrl * location * ctrl * ctrlType
    | Branch of ctrl * ctrl * ctrl * ctrlType
    | Choose of direction * location * ctrl * ctrlType
    | AllowL of location * ctrl * ctrlType
    | AllowR of location * ctrl * ctrlType
    | AllowLR of location * ctrl * ctrl * ctrlType
    | Let of ctrl * ctrl * ctrl * ctrlType
    | Fun of name * ctrl * ctrl * ctrlType
    (* | Calling of name * ctrl * ctrlType *)
    | Application of ctrl * ctrl * ctrlType
  [@@deriving show]

  (* *************Methods************ *)
  let rec get_ctrlGType (typ : globalType) : ctrlType =
    match typ with
    | DotType(_, typ) -> get_ctrlLType typ
    | ArrowType(ityp, otyp) -> CtrlFun (get_ctrlGType ityp, get_ctrlGType otyp) 

  let rec ctrlType_equal a b = (match (a, b) with
    | (Int, Int) -> true
    | (String, String) -> true
    | (Bool, Bool) -> true
    | (CtrlFun (gt11, gt12), CtrlFun (gt21, gt22)) ->
      ctrlType_equal gt11 gt21 && ctrlType_equal gt12 gt22
    | _ -> false
  )

  let rec merge_branch (lbranch:ctrl) (rbranch:ctrl) : ctrl option = (match lbranch, rbranch with
    | ChoreoVars (x, x_typ), ChoreoVars (y, y_typ) 
      when x = y && ctrlType_equal x_typ y_typ -> Some (ChoreoVars (x, x_typ))
    | Unit, Unit -> Some Unit
    | Ret (x, x_typ), Ret (y, y_typ) when x = y && ctrlType_equal x_typ y_typ -> Some (Ret (x, x_typ))
    | Branch (ift, thn, el, x_typ), Branch (ift2, thn2, el2, y_typ) 
      when ift = ift2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        let merged_el = merge_branch el el2 in
        (match merged_thn, merged_el with
            | Some merged_thn, Some merged_el -> Some (Branch (ift, merged_thn, merged_el, x_typ))
            | _ -> None)
    | Snd (arg, loc, thn, x_typ), Snd (arg2, loc2, thn2, y_typ) 
      when arg = arg2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Snd (arg, loc, merged_thn, x_typ))
          | _ -> None)
    | Rcv (arg, loc, thn, x_typ), Rcv (arg2, loc2, thn2, y_typ) 
    when arg = arg2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
      let merged_thn = merge_branch thn thn2 in
      (match merged_thn with
        | Some merged_thn -> Some (Rcv (arg, loc, merged_thn, x_typ))
        | _ -> None)
    | Choose (d, loc, thn, x_typ), Choose (d2, loc2, thn2, y_typ) 
      when d = d2 && loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (Choose (d, loc, merged_thn, x_typ))
          | _ -> None)
    (* LL *)
    | AllowL (loc, thn, x_typ), AllowL (loc2, thn2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thn thn2 in
        (match merged_thn with
          | Some merged_thn -> Some (AllowL (loc, merged_thn, x_typ))
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
          | Some merged_thn -> Some (AllowLR (loc, merged_thn, thnR, lr_typ))
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
          | Some merged_thn -> Some (AllowR (loc, merged_thn, x_typ))
          | _ -> None)
    (* RLR *)
    | AllowR (loc, thn, x_typ), AllowLR (loc2, thnL, thnR, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ  ->
        let merged_thn = merge_branch thn thnR in
        (match merged_thn with
          | Some merged_thn -> Some (AllowLR (loc, thnL, merged_thn, y_typ))
          | _ -> None)
    (* LRL *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowL (loc2, thn, y_typ)
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thnL thn in
        (match merged_thn with
          | Some merged_thn -> Some (AllowLR (loc, merged_thn, thnR, x_typ))
          | _ -> None)
    (* LRR *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowR (loc2, thn, y_typ)
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn = merge_branch thnR thn in
        (match merged_thn with
          | Some merged_thn -> Some (AllowLR (loc, thnL, merged_thn, x_typ))
          | _ -> None)
    (* LRLR *)
    | AllowLR (loc, thnL, thnR, x_typ), AllowLR (loc2, thnL2, thnR2, y_typ) 
      when loc = loc2 && ctrlType_equal x_typ y_typ ->
        let merged_thn_l = merge_branch thnL thnL2 in
        let merged_thn_r = merge_branch thnR thnR2 in
        (match merged_thn_l, merged_thn_r with
          | Some merged_thn_l, Some merged_thn_r -> Some (AllowLR (loc, merged_thn_l, merged_thn_r, x_typ))
          | _ -> None)
    | Let (binder, arg, thn, x_typ), Let (binder2, arg2, thn2, y_typ)
        when binder = binder2 && ctrlType_equal x_typ y_typ -> 
        let merged_arg = merge_branch arg arg2 in
        let merged_thn = merge_branch thn thn2 in
        (match merged_arg, merged_thn with
          | Some merged_arg, Some merged_thn -> Some (Let (binder, merged_arg, merged_thn, x_typ))
          | _ -> None)
    | Fun (name, arg, body, x_typ), Fun (name2, arg2, body2, y_typ) 
        when name = name2 && arg = arg2 && body = body2 && ctrlType_equal x_typ y_typ ->
          Some (Fun (name, arg, body, x_typ))
    | Application (funct, argument, x_typ), Application (funct2, argument2, y_typ) 
      when ctrlType_equal x_typ y_typ ->
      let merged_funct = merge_branch funct funct2 in
      let merged_argument = merge_branch argument argument2 in
      (match merged_funct, merged_argument with
          | Some merged_funct, Some merged_argument -> Some (Application (merged_funct, merged_argument, x_typ))
          | _ -> None)
    | _ -> None
  )

  let rec expr_to_ctrl (expr_ast: expr) (currentNode: location): ctrl option =
    match expr_ast with
    | Assoc (loc, arg, Some typ) ->
      (match lexpr_to_lctrl arg with
          | Some arg when currentNode = loc -> Some (Ret (arg, (get_ctrlGType typ)))
          | Some _ -> Some Unit
          | _ -> None)
    | Branch (Assoc(loc, arg, assoc_typ), thn, el, Some typ) -> 
        (match expr_to_ctrl (Assoc(loc, arg, assoc_typ)) currentNode, expr_to_ctrl thn currentNode, expr_to_ctrl el currentNode with
          | Some ift, Some thn, Some el 
              when currentNode = loc -> 
                Some (Branch (ift, thn, el, (get_ctrlGType typ)))
          | Some _, Some parsed_thn, Some parsed_el -> 
              (match merge_branch parsed_thn parsed_el with
              | None -> None
              | Some x -> Some x)
          | _ -> None)
    | Sync (sndr, Direction d, rcvr, thn, Some typ) ->
      if currentNode = sndr && currentNode = rcvr then
        None
      else if currentNode = sndr && currentNode != rcvr then
        (match expr_to_ctrl thn currentNode with
          |Some thn -> Some (Choose (Direction d, rcvr, thn, get_ctrlGType typ))
          | _ -> None)
      else if currentNode = rcvr && currentNode != sndr then
        (match expr_to_ctrl thn currentNode, d with
          | Some thn, "L"-> Some (AllowL (sndr, thn, get_ctrlGType typ))
          | Some thn, "R"-> Some (AllowR (sndr, thn, get_ctrlGType typ))
          | _ -> None)
      else
        expr_to_ctrl thn currentNode
  
    | Let (_, bndr_arg, Snd (Assoc (loc, arg_snd, Some _), rcvr, Some _), thn, Some thn_typ)->
      let parsed_thn = expr_to_ctrl thn currentNode in
      let parsed_bndr_arg = lexpr_to_lctrl bndr_arg in
      let parsed_arg_snd = lexpr_to_lctrl arg_snd in
      (match parsed_thn, parsed_bndr_arg, parsed_arg_snd with
        | Some parsed_thn, Some _, Some parsed_arg_snd when rcvr != currentNode && currentNode = loc
          -> Some (Snd (parsed_arg_snd, rcvr, parsed_thn, (get_ctrlGType thn_typ)))
        | Some parsed_thn, Some parsed_bndr_arg, Some _ when rcvr = currentNode && currentNode != loc 
          -> Some (Rcv (parsed_bndr_arg, loc, parsed_thn, (get_ctrlGType thn_typ)))
        | Some parsed_thn, _, _ when rcvr != currentNode && currentNode != loc -> Some parsed_thn
        | _ -> None)

      (* Sender is choreovar X~>p2.x*)
    (* | Let (_, bndr_arg, Snd (ChoreoVars (Name name, Some _), rcvr, Some _), thn, Some thn_typ)->
      let parsed_thn = expr_to_ctrl thn currentNode in
      let parsed_bndr_arg = lexpr_to_lctrl bndr_arg in
      let parsed_arg_snd = lexpr_to_lctrl arg_snd in
      (match parsed_thn, parsed_bndr_arg, parsed_arg_snd with
        | Some parsed_thn, Some _, Some parsed_arg_snd when rcvr != currentNode && currentNode = loc
          -> Some (Snd (parsed_arg_snd, rcvr, parsed_thn, (get_ctrlGType thn_typ)))
        | Some parsed_thn, Some parsed_bndr_arg, Some _ when rcvr = currentNode && currentNode != loc 
          -> Some (Rcv (parsed_bndr_arg, loc, parsed_thn, (get_ctrlGType thn_typ)))
        | Some parsed_thn, _, _ when rcvr != currentNode && currentNode != loc -> Some parsed_thn
        | _ -> None) *)

  
    | Let (loc, Variable(var_name, Some var_typ), snd, thn, Some typ) ->
      (match expr_to_ctrl (Assoc(loc, Variable(var_name, Some var_typ), Some (DotType(loc, var_typ)))) currentNode, expr_to_ctrl snd currentNode, expr_to_ctrl thn currentNode with
        | Some parsed_arg, Some parsed_snd, Some parsed_thn when loc = currentNode ->
          Some (Let (parsed_arg, parsed_snd, parsed_thn, (get_ctrlGType typ)))
        | Some _, Some parsed_snd, Some parsed_thn when loc != currentNode ->
          Some (Let (Unit, parsed_snd, parsed_thn, (get_ctrlGType typ)))
        | _ -> None)
    (* remove the local function implementation*)
    (* | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
      let parsed_body = expr_to_ctrl body currentNode in
      let parsed_arg = expr_to_ctrl arg2 currentNode in
      (match parsed_body, parsed_arg with 
        | Some parsed_body, Some parsed_arg when loc = currentNode ->
          Some (Fun {name ; arg = parsed_arg; body = parsed_body})
        | Some parsed_body, Some _ when loc != currentNode ->
          Some (Fun {name ; arg = ChoreoVars (get_fresh_cname()); body = parsed_body})
        | _ -> None) *)
    | Fun (name, arg, body, Some typ) -> 
      (match expr_to_ctrl arg currentNode, expr_to_ctrl body currentNode with
        | Some argument, Some parsed_body -> Some (Fun (name, argument, parsed_body, (get_ctrlGType typ)))
        | _ -> None)
    (* | Calling {name; arg} ->  
      let parsed_arg = expr_to_ctrl arg currentNode in
      (match parsed_arg with
       | Some parsed_arg -> Some (Calling {name; arg = parsed_arg})
       | _ -> None) *)
    | Application (funct, argument, Some typ) ->
        (match expr_to_ctrl funct currentNode, expr_to_ctrl argument currentNode with
          | Some parsed_funct, Some parsed_argument ->
            Some (Application (parsed_funct, parsed_argument, (get_ctrlGType typ)))
          | _ -> None)
    | ChoreoVars (x, Some typ) -> Some (ChoreoVars (x, (get_ctrlGType typ)))
    | Snd _ -> None
    | _ -> raise (EndpointProjectionFailedException "Case not executed")
end      
  
