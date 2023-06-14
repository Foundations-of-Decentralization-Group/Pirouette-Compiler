open Ctrl
open Expr

module SS = Set.Make(String);;

(* let get_fresh_fname =
  let counter = ref 0 in
  fun () ->
    let name = "func_" ^ string_of_int !counter in
    counter := !counter + 1;
    name

let get_fresh_cname =
  let counter = ref 0 in
  fun () ->
    let name = "X_" ^ string_of_int !counter in
    counter := !counter + 1;
    name *)
(* let check_str s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false *)

(* let parse_eq parsed_lft parsed_rght op : string = 
  let isStrL = check_str parsed_lft in
  let isStrR = check_str parsed_rght in
  if isStrL = true &&  isStrR = true then
  "(" ^ parsed_lft ^ " " ^ op ^ " " ^ parsed_rght ^ ")"
  else parsed_lft ^ " " ^ op ^ " " ^ parsed_rght *)
(* 
let rec parse_expr expr currentNode : string =
  match expr with
  | Branch { ift = Assoc { loc; arg }; thn; el } ->
      let parsed_arg = parse_expr arg currentNode in
      let parsed_thn = parse_expr thn currentNode in
      let parsed_el = parse_expr el currentNode in
      if currentNode = loc then 
        "if " ^ parsed_arg ^ " then " ^ parsed_thn ^ " else " ^ parsed_el
    else
      " ********** MERGE REQD ********** "
  | Branch { ift = _; thn = _; el = _} -> ""
  | Sync { sndr; d; rcvr; thn} ->
    if currentNode = sndr && currentNode = rcvr then
      ""
    else if currentNode = sndr && currentNode != rcvr then
      let parsed_el = parse_expr thn currentNode in
        "choose " ^ d ^ " for " ^ rcvr ^ "; \n" ^ parsed_el
    else if currentNode = rcvr && currentNode != sndr then
      let parsed_el = parse_expr thn currentNode in
        "allow " ^ sndr ^ " choice | " ^ d ^ " => " ^ parsed_el
    else
      parse_expr thn currentNode
  | Assoc { loc; arg } ->
      let parsed_arg = parse_expr arg currentNode in
      if currentNode = loc then
        "ret(" ^ parsed_arg ^ ")"
      else 
        "()"
  | Variable x -> x
  | Plus {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "+"
  | Minus {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "-"
  | Product {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "*"
  | Division {lft; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght "/"
  | Condition {lft; op; rght} -> 
    let parsed_lft = parse_expr lft currentNode in
    let parsed_rght = parse_expr rght currentNode in
    parse_eq parsed_lft parsed_rght op
  | Value x -> string_of_int x
  | Map {name; arg} -> 
    let parsed_arg = parse_expr arg currentNode in
    name ^ "[" ^ parsed_arg ^ "]"
  | Let {fst = Assoc{loc = _; arg = arg_fst}; snd = Snd {sndr = Assoc {loc; arg = arg_snd}; name}; thn} ->
    let parsed_thn = parse_expr thn currentNode in
    let parsed_arg_fst = parse_expr arg_fst currentNode in
    let parsed_arg_snd = parse_expr arg_snd currentNode in
    if name = currentNode && name = loc then ""
    else if name != currentNode && currentNode = loc then
      "send " ^ parsed_arg_snd ^ " to " ^ name ^ "; \n" ^ parsed_thn
    else if name = currentNode && currentNode != loc then
      "receive " ^ parsed_arg_fst ^ " loc " ^ loc ^ "; \n" ^ parsed_thn
    else
      parsed_thn
  | Let {fst = Assoc{loc; arg}; snd; thn} ->
    let parsed_arg = parse_expr arg currentNode in
    let parsed_snd = parse_expr snd currentNode in 
    let parsed_thn = parse_expr thn currentNode in
    if loc = currentNode then 
      "let ret(" ^ parsed_arg ^ ") := " ^ parsed_snd ^ " in " ^ parsed_thn
    else "( fun_g F(X) := " ^ parsed_thn ^ ") " ^ parsed_snd
  | Let {fst = _; snd = _; thn = _} -> ""
  | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
    let parsed_body = parse_expr body currentNode in
    let parsed_arg = parse_expr arg2 currentNode in
    if loc = currentNode then
      "fun_l " ^ name ^ "(" ^ parsed_arg ^ ") := " ^ parsed_body
    else
      "fun_g " ^ name ^ "(X) := " ^ parsed_body
  | Fun {name; arg = ChoreoVars x; body} -> 
    let parsed_body = parse_expr body currentNode in
    "fun_g " ^ name ^ "("^ x ^") := " ^ parsed_body
  | Fun {name = _; arg = _; body = _} -> ""
  | Application {funct; argument = Assoc {loc; arg}} -> 
    let parsed_funct = parse_expr funct currentNode in
    let parsed_arg = parse_expr arg currentNode in
    if loc = currentNode then
      parsed_funct ^ " " ^ parsed_arg
    else
      parsed_funct ^ " ()"
  | Application {funct; argument} -> 
    let parsed_funct = parse_expr funct currentNode in
    let parsed_argument = parse_expr argument currentNode in
      parsed_funct ^ " " ^ parsed_argument
  | ChoreoVars x -> x 
  | Snd _ | Abstraction _ | Comm_S _ | UMinus _ -> ""  *)

let get_entitities expr : SS.t = 
  let set1 = SS.empty in
    let rec aux acc expr = match expr with
      | Branch { ift; thn; el } -> 
        let acc_ift = aux acc ift in
        let acc_thn = aux acc_ift thn in
        let acc_el = aux acc_thn el in
          acc_el
      | Sync {sndr; d = _; rcvr; thn} ->
        let acc = aux acc thn in
          let acc = SS.union (SS.add sndr acc) (SS.add rcvr acc) in 
            acc
      | Assoc {loc; arg = _} ->
          (SS.add loc acc)
      | Fun {name = _; arg; body} ->
        let acc_arg = aux acc arg in
        let acc = aux acc_arg body in
          acc
      | Snd {sndr; name} -> 
        let acc_sndr = aux acc sndr in
          (SS.add name acc_sndr)
      | Let {fst; snd; thn}  ->
        let acc_fst = aux acc fst in
        let acc_snd = aux acc_fst snd in
        let acc_thn = aux acc_snd thn in
          acc_thn
      | Application {funct; argument} -> 
        let acc_funct = aux acc funct in
        let acc = aux acc_funct argument in
          acc
      | Variable _ | Value _ | ChoreoVars _ | Condition _ | Map _ | Abstraction _
      | Comm_S _ | Plus _|Minus _|Product _|Division _| UMinus _-> acc
    in
      aux set1 expr

(* let ast = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
snd =
Expr.Snd {
  sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")};
  name = "Person2"};
thn =
Expr.Application {
  funct =
  Expr.Fun {name = "initpay"; arg = (Expr.ChoreoVars "X");
    body =
    Expr.Branch {
      ift =
      Expr.Assoc {loc = "Person2";
        arg =
        Expr.Condition {lft = (Expr.Variable "d"); op = "<";
          rght = (Expr.Value 500)}};
      thn =
      Expr.Branch {
        ift =
        Expr.Assoc {loc = "Person2";
          arg =
          Expr.Condition {lft = (Expr.Variable "d"); op = "<";
            rght = (Expr.Value 300)}};
        thn =
        Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1";
          thn = (Expr.ChoreoVars "X")};
        el =
        Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1";
          thn = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)}}};
      el =
      Expr.Branch {
        ift =
        Expr.Assoc {loc = "Person2";
          arg =
          Expr.Condition {lft = (Expr.Variable "d"); op = "<";
            rght = (Expr.Value 300)}};
        thn =
        Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1";
          thn = (Expr.ChoreoVars "X")};
        el =
        Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1";
          thn = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)}}}}};
  argument = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "d")}}} *)
let ast = (Expr.Let {                          
  fst = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")};
  snd = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "\"5\"")};
  thn =
  Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
    snd =
    Expr.Snd {
      sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")};
      name = "Person2"};
    thn = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}})

let entities : SS.t = 
  get_entitities ast

let rec merge_branch (lbranch:ctrl) (rbranch:ctrl) : ctrl option= 
  match lbranch, rbranch with
    | ChoreoVars x, ChoreoVars y when x = y -> Some (ChoreoVars x)
    | Unit, Unit -> Some Unit
    | Ret x, Ret y when x = y -> Some (Ret x)
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

  (* let rec parse_ast expr currentNode = *)
let rec parse_ast (expr_ast: expr) (currentNode: string): ctrl option =
  match expr_ast with
  | Assoc { loc; arg } ->
    let parsed_arg = parse_ast arg currentNode in
    (match parsed_arg with
        | Some parsed_arg when currentNode = loc -> Some (Ret parsed_arg)
        | Some _ -> Some Unit
        | _ -> None)
  | Branch { ift = Assoc { loc; arg }; thn; el } ->
      let parsed_arg = parse_ast arg currentNode in
      let parsed_thn = parse_ast thn currentNode in
      let parsed_el = parse_ast el currentNode in
      (match parsed_arg, parsed_thn, parsed_el with
        | Some parsed_arg, Some parsed_thn, Some parsed_el 
            when currentNode = loc -> 
              Some (Ctrl.Branch {ift = parsed_arg; thn = parsed_thn; el = parsed_el})
        | Some _, Some parsed_thn, Some parsed_el -> 
            (match merge_branch parsed_thn parsed_el with
            | None -> None
            | Some x -> Some x)
        | _ -> None)
  | Branch { ift = _; thn = _; el = _} -> None
  | Sync { sndr; d; rcvr; thn} ->
    if currentNode = sndr && currentNode = rcvr then
      None
    else if currentNode = sndr && currentNode != rcvr then
      let parsed_el = parse_ast thn currentNode in
      (match parsed_el with
        |Some parsed_el -> Some (Choose {d; loc = rcvr; thn = parsed_el})
        | _ -> None)
    else if currentNode = rcvr && currentNode != sndr then
      let parsed_el = parse_ast thn currentNode in
      (match parsed_el, d with
        | Some parsed_el, "L"-> Some (AllowL {loc= sndr; thn = parsed_el})
        | Some parsed_el, "R"-> Some (AllowR {loc= sndr; thn = parsed_el})
        | _ -> None)
    else
      parse_ast thn currentNode
  | Variable x -> Some (Variable x)
  | Plus {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    (match parsed_lft, parsed_rght with
      | Some parsed_lft, Some parsed_rght -> 
        Some (Ctrl.Plus {lft = parsed_lft; rght = parsed_rght})
      | _ -> None)
    
  | Minus {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    (match parsed_lft, parsed_rght with
      | Some parsed_lft, Some parsed_rght -> 
        Some (Ctrl.Minus {lft = parsed_lft; rght = parsed_rght})
      | _ -> None)
  | Product {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    (match parsed_lft, parsed_rght with
      | Some parsed_lft, Some parsed_rght -> 
        Some (Ctrl.Product {lft = parsed_lft; rght = parsed_rght})
      | _ -> None)
  | Division {lft; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    (match parsed_lft, parsed_rght with
      | Some parsed_lft, Some parsed_rght -> 
        Some (Ctrl.Division {lft = parsed_lft; rght = parsed_rght})
      | _ -> None)
  | Condition {lft; op; rght} -> 
    let parsed_lft = parse_ast lft currentNode in
    let parsed_rght = parse_ast rght currentNode in
    (match parsed_lft, parsed_rght with
      | Some parsed_lft, Some parsed_rght -> 
        Some (Ctrl.Condition {lft = parsed_lft; op; rght = parsed_rght})
      | _ -> None)
  | Value x -> Some (Value x)
  | Map {name; arg} -> 
    let parsed_arg = parse_ast arg currentNode in
    (match parsed_arg with
      | Some parsed_arg -> Some (Ctrl.Map {name; arg = parsed_arg})
      | _-> None)
  | Let {fst = Assoc{loc = _; arg = arg_fst}; snd = Snd {sndr = Assoc {loc; arg = arg_snd}; name}; thn} ->
    let parsed_thn = parse_ast thn currentNode in
    let parsed_arg_fst = parse_ast arg_fst currentNode in
    let parsed_arg_snd = parse_ast arg_snd currentNode in
    (match parsed_thn, parsed_arg_fst, parsed_arg_snd with
      | Some parsed_thn, Some _, Some parsed_arg_snd when name != currentNode && currentNode = loc
        -> Some (Ctrl.Snd {arg = parsed_arg_snd; loc = name; thn = parsed_thn})
      | Some parsed_thn, Some parsed_arg_fst, Some _ when name = currentNode && currentNode != loc
      -> Some (Ctrl.Rcv {arg = parsed_arg_fst; loc; thn = parsed_thn})
      | _, _, _ when name = currentNode && name = loc -> None
      | Some parsed_thn, _, _ when name != currentNode && currentNode != loc -> Some parsed_thn
      | _ -> None)
  (* remove the fresh thing -> intead of function put let () = E1 in E2 in ctrl
      and let _ = E1 in E2 for ocaml  *)
  | Let {fst = Assoc{loc; arg}; snd; thn} ->
    let parsed_arg = parse_ast arg currentNode in
    let parsed_snd = parse_ast snd currentNode in 
    let parsed_thn = parse_ast thn currentNode in
    (match parsed_arg, parsed_snd, parsed_thn with
      | Some parsed_arg, Some parsed_snd, Some parsed_thn when loc = currentNode ->
        Some (Ctrl.Let {binder = parsed_arg; arg = parsed_snd; thn = parsed_thn})
      | Some _, Some parsed_snd, Some parsed_thn when loc != currentNode ->
        Some (Ctrl.Let {binder = Unit; arg = parsed_snd; thn = parsed_thn})
      | _ -> None)
  | Let {fst = _; snd = _; thn = _} -> None
  (* remove the local function implementation*)
  (* | Fun {name; arg = Assoc {loc; arg = arg2}; body} -> 
    let parsed_body = parse_ast body currentNode in
    let parsed_arg = parse_ast arg2 currentNode in
    (match parsed_body, parsed_arg with 
      | Some parsed_body, Some parsed_arg when loc = currentNode ->
        Some (Ctrl.Fun {name ; arg = parsed_arg; body = parsed_body})
      | Some parsed_body, Some _ when loc != currentNode ->
        Some (Ctrl.Fun {name ; arg = ChoreoVars (get_fresh_cname()); body = parsed_body})
      | _ -> None) *)
  | Fun {name; arg = ChoreoVars x; body} -> 
    let parsed_body = parse_ast body currentNode in
    (match parsed_body with
      | Some parsed_body -> Some (Ctrl.Fun {name = name ; arg = ChoreoVars x; body = parsed_body})
      | _ -> None)
  | Fun {name = _; arg = _; body = _} -> None
  | Application {funct; argument} -> 
    let parsed_funct = parse_ast funct currentNode in
    let parsed_argument = parse_ast argument currentNode in
      (match parsed_funct, parsed_argument with
        | Some parsed_funct, Some parsed_argument ->
          Some (Ctrl.Application {funct = parsed_funct; argument = parsed_argument})
        | _ -> None)
  | ChoreoVars x -> Some (ChoreoVars x)
  | Snd _ | Abstraction _ | Comm_S _ | UMinus _ -> None 

let () = SS.iter (fun entity -> 
  let res = parse_ast ast entity in
  let str = "____________________________" ^ entity ^ "___________________________________" in 
  print_endline str;
  let r = match res with
    | Some res -> show_ctrl res
    | None -> ""
  in Printf.printf "%s\n\n" r
  ) entities