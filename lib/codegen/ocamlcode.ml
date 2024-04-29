open Ast

(* open Sum *)
let rec local_type (t : Local.typ) : string =
  match t with
  | TUnit -> "unit"
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"
  | TProd (t1, t2) -> local_type t1 ^ "*" ^ local_type t2
  | TSum (t1, t2) -> "( " ^ local_type t1 ^ ", " ^ local_type t2 ^ ") sum"

and local_pattern (p : Local.pattern) : string =
  match p with
  | Default -> "_"
  | Val v -> (
      match v with
      | `Int n -> Printf.sprintf "%d" n
      | `String n -> n
      | `Bool n -> Printf.sprintf "%b" n)
  | Var (VarId id) -> id
  | Pair (p1, p2) -> "(" ^ local_pattern p1 ^ ", " ^ local_pattern p2 ^ ")"
  | Left (Pair (l, _)) -> local_pattern l
  | Right (Pair (_, r)) -> local_pattern r
  | _ -> "()"

and local_expr (e : Local.expr) : string =
  match e with
  | Unit -> "()"
  | Val v -> (
      match v with
      | `Int n -> Printf.sprintf "%d" n
      | `String n -> n
      | `Bool n -> Printf.sprintf "%b" n)
  | Var (VarId id) -> id
  | UnOp (op, e) -> (match op with Not -> "not " | Neg -> "-") ^ local_expr e
  | BinOp (e1, op, e2) ->
      "(" ^ local_expr e1
      ^ (match op with
        | Plus -> "+"
        | Minus -> "-"
        | Times -> "*"
        | Div -> "/"
        | And -> "&&"
        | Or -> "||"
        | Eq -> "="
        | Neq -> "!="
        | Lt -> "<"
        | Leq -> "<="
        | Gt -> ">"
        | Geq -> ">=")
      ^ local_expr e2 ^ ")"
  | Let (VarId id, e1, e2) ->
      "let " ^ id ^ " = " ^ local_expr e1 ^ " in " ^ local_expr e2
  | Pair (e1, e2) -> "(" ^ local_expr e1 ^ ", " ^ local_expr e2 ^ ")"
  | Fst e -> "fst" ^ local_expr e
  | Snd e -> "snd" ^ local_expr e
  | Left (Pair (l, _)) -> "left" ^ local_expr l
  | Right (Pair (_, r)) -> "right" ^ local_expr r
  | Match (e, cases) ->
      "match" ^ local_expr e ^ " with "
      ^ List.fold_left
          (fun acc (p, e') ->
            acc ^ "| " ^ local_pattern p ^ " -> " ^ local_expr e')
          "" cases
  | _ -> "()"

let rec stmt_blocks (stmts : Net.stmt_block) =
  List.fold_left (fun acc stmt -> acc ^ netir_stmt stmt) "" stmts

and netir_stmt = function
  | Decl (_, _) -> " "
  | Assign (ps, e) ->
      "let"
      ^ List.fold_left (fun acc p -> acc ^ local_pattern p) "" ps
      ^ " = " ^ netir_expr e
  | TypeDecl (TypId id, t) -> "type " ^ id ^ netir_type t

and netir_type = function
  | TUnit -> "unit"
  | TLoc t -> local_type t
  | TMap (t1, t2) -> netir_type t1 ^ " -> " ^ netir_type t2
  | TProd (t1, t2) -> netir_type t1 ^ " * " ^ netir_type t2
  | TSum (t1, t2) -> netir_type t1 ^ " + " ^ netir_type t2

and netir_expr _ = "()"
(*function
  | Unit -> "()"
  | Var (VarId id) ->
      Printf.sprintf "%d" id
  | Fst e ->
      "fst" ^ local_expr e
  | Snd e ->
      "snd" ^ local_expr e
  | Left e ->
      ""
  | Right e ->
      " "
  | Send (e, LocId loc) ->
      " "
  | If (e1, e2, e3) ->
      " if " ^
          (netir_expr e1)^
      " then " ^
          (netir_expr e2) ^
      " else " ^
          (netir_expr e3)
  | Let (stmt_block, e) -> " "

  | FunDef (p, e) ->  " "

  | FunApp (e1, e2) -> " "

  | Pair (e1, e2) ->
      "("^ netir_expr e1 ^", "^ netir_expr e2 ^")"
  | Ret (e) ->
      local_expr e
  | AllowChoice (Locid loc, choices) -> " "
  | Recv (LocId loc) -> " "
  | ChooseFor (LabelId id, LocId locid, e) -> " "
  | Match (e, cases) ->
      "match" ^ netir_expr e ^ " with "
      let match_cases cases =
        List.iter (fun (p, e') -> "| " ^ (netir_pattern p) ^ " -> " ^ (netir_expr e')) *)

let ocamlcode_netir channel (Net.Prog prog) =
  Printf.fprintf channel "%s" (stmt_blocks prog)
