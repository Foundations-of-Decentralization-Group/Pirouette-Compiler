open Expr
(* open Set *)
(* open List *)

(* module SS = Set.Make(String);;

let set1 = SS.singleton "hello";;

let set1 = List.fold_right SS.add ["hello"; "world"; "community"; "manager";
"stuff"; "blue"; "green"] set1;;

let print_set s =
  SS.iter print_endline s;;

let () = print_set set1 *)

(* Custom parser function *)
(* let rec parse_expr expr currentNode : string =
  match expr with
 |  Branch { ift = Assoc { loc; arg }; thn; el } ->
      let parsed_arg = parse_expr arg currentNode in
      let parsed_thn = parse_expr thn currentNode in
      let parsed_el = parse_expr el currentNode in
      if currentNode = loc then 
        "if " ^ parsed_arg ^ " then " ^ parsed_thn ^ " else " ^ parsed_el
    else
      parsed_thn ^ parsed_el
  | Branch { ift; thn; el } -> let parsed_arg = parse_expr ift currentNode in
    let parsed_thn = parse_expr thn currentNode in
    let parsed_el = parse_expr el currentNode in
    "if " ^ parsed_arg ^ " then " ^ parsed_thn ^ " else " ^ parsed_el
  | Sync { sndr; d; rcvr; } ->
    "choose" ^ d ^ " for " ^ rcvr ^ ";" ^ sndr
    (* if currentNode = sndr && currentNode = rcvr then
      ""
    else if currentNode = sndr && currentNode != rcvr then
      let parsed_el = parse_expr thn currentNode in
        "choose" ^ d ^ " for " ^ rcvr ^ ";" ^ parsed_el
    else  *)
        (* "()" *)
  | Assoc { loc; arg } ->
      let parsed_arg = parse_expr arg currentNode in
      if currentNode = loc then
        "ret(" ^ parsed_arg ^ ")"
      else 
        "()"
  | Variable x -> x
  | Value _ | ChoreoVars _ | Condition _ | Fun _ | Seq _ | Snd _ | Let _ | Map _ | Abstraction _
  | Application _ | Comm_S _ | Plus _|Minus _|Product _|Division _|UMinus _->
      ""  *)

module SS = Set.Make(String);;

let print_set s =
  SS.iter print_endline s;;

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
      | Variable _ | Value _ | ChoreoVars _ | Condition _ | Seq _  | Map _ | Abstraction _
      | Comm_S _ | Plus _|Minus _|Product _|Division _| UMinus _-> acc
    in
      aux set1 expr

let identifiers : SS.t =
  get_entitities
  (Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
  snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person3"; arg = (Expr.Variable "amt_due")}; name = "Person4"};
  thn = Expr.Application { funct = Expr.Fun {name = "initpay"; arg = (Expr.ChoreoVars "X");
  body = Expr.Branch { ift = Expr.Assoc {loc = "Person5"; arg = Expr.Condition {lft = (Expr.Variable "x"); op = "<"; rght = (Expr.Value 500)}};
  thn = Expr.Sync {sndr = "Person6"; d = "L"; rcvr = "Person7"; thn = Expr.Let { fst = Expr.Assoc {loc = "Person8"; arg = (Expr.Variable "rcv")};
  snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person9"; arg = (Expr.Variable "rem")}; name = "Person10"};
  thn = Expr.Let { fst = Expr.Assoc {loc = "Person11"; arg = (Expr.Variable "y")};
  snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person12"; arg = Expr.Minus {lft = (Expr.Variable "amt_due");  rght = (Expr.Variable "rem")}}; 
  name = "Person14"}; thn = Expr.Assoc {loc = "Person13"; arg = (Expr.Variable "d")}}}};
  el = Expr.Sync {sndr = "Person16"; d = "R"; rcvr = "Person15"; thn = Expr.Let {fst = Expr.Assoc {loc = "Person17"; arg = (Expr.Variable "rcv")};
  snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person18"; arg = (Expr.Value 500)}; name = "Person19"};
  thn = Expr.Let {fst = Expr.Assoc {loc = "Person20"; arg = (Expr.Variable "y")}; 
  snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person21"; arg = (Expr.Value 0)}; name = "Person22"};thn = (Expr.ChoreoVars "X")}}}}};
  argument = Expr.Assoc {loc = "Person23"; arg = (Expr.Variable "d")}}})
(* Print the parsed expression *)
let () = print_set identifiers


(* let parsed_expr : string =
  parse_expr
    (Expr.Branch {ift = Expr.Assoc {loc = "l"; arg = (Expr.Variable "e")};
      thn = Expr.Sync {sndr = "Buyer"; d = "L"; rcvr = "Seller"};        
      el = Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller"}}) "Buyer" *)