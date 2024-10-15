open Ast

let node_counter = ref 0

(** [generate_node_name] generates a node name for the dot code

    - This function reads the current node counter, increments it, and concatentates it with the node_id to create a node name.
    - Returns: A string that represents a node name in the dot code. *)
let generate_node_name () =
  let node_id = !node_counter in
  node_counter := !node_counter + 1;
  "n" ^ string_of_int node_id
;;

(* node name format: n + node_counter *)

(** [dot_local_pattern patn] creates the dot code for local patterns [patn]

    - [patn] is a local pattern.
    - Variants of local patterns include Default, Val, Var, Pair, Left, and Right.
    - For variants [Pair], [Left], and [Right], it calls helper function [dot_local_pattern] to generate the dot code for the local pattern.
    - Connects the local pattern node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local pattern [patn] and the
      second element is the node name of the local pattern [patn]. *)
let rec dot_local_pattern (patn : Local.pattern) : string * string =
  let node_name = generate_node_name () in
  match patn with
  | Default (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"Default\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Val (v, _) ->
    (match v with
     | Int (i, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           (string_of_int i)
           line
           start_idx
           end_idx
       , node_name )
     | String (s, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           s
           line
           start_idx
           end_idx
       , node_name )
     | Bool (b, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           (string_of_bool b)
           line
           start_idx
           end_idx
       , node_name ))
  | Var (VarId (id, _), (_, line, start_idx, end_idx)) ->
    ( Printf.sprintf
        "%s [label=\"%s\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        id
        line
        start_idx
        end_idx
    , node_name )
  | Pair (patn1, patn2, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_pattern patn1 in
    let c2, n2 = dot_local_pattern patn2 in
    let pair_node =
      Printf.sprintf
        "%s [label=\"Pair\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Left (patn, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_pattern patn in
    let left_node =
      Printf.sprintf
        "%s [label=\"Left\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (patn, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_pattern patn in
    let right_node =
      Printf.sprintf
        "%s [label=\"Right\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name

(** [dot_local_type typ] creates the dot code for local types [typ]

    - [typ] is a local type.
    - Variants of local types include TUnit, TInt, TString, TBool, TProd, and TSum.
    - For variants [TProd] and [TSum], it calls helper function [dot_local_type] to generate the dot code for the local type.
    - Connects the local type node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local type [typ] and the
      second element is the node name of the local type [typ]. *)
and dot_local_type (typ : Local.typ) : string * string =
  let node_name = generate_node_name () in
  match typ with
  | TUnit (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"()\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | TInt (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"Int\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | TString (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"String\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | TBool (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"Bool\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | TProd (typ1, typ2, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_type typ1 in
    let c2, n2 = dot_local_type typ2 in
    let prod_node =
      Printf.sprintf
        "%s [label=\"Product\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    prod_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | TSum (typ1, typ2, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_type typ1 in
    let c2, n2 = dot_local_type typ2 in
    let sum_node =
      Printf.sprintf
        "%s [label=\"Sum\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    sum_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name

(** [dot_local_expr loc_expr] creates the dot code for local expressions [loc_expr]

    - [loc_expr] is a local expression.
    - Variants of local expressions include Unit, Val, Var, BinOp, Let, Pair, Fst, Snd, Left, Right, and Match.
    - Calls helper functions [dot_bin_op], [dot_local_cases], [dot_local_case], [dot_local_pattern], and [dot_local_expr]
      to generate the dot code for the local expression.
    - Connects the local expression node to it's children.
    - Returns: A tuple of strings where the first element is the dot code for the local expression [loc_expr] and the
      second element is the node name of the local expression [loc_expr]. *)
and dot_local_expr (loc_expr : Local.expr) : string * string =
  let node_name = generate_node_name () in
  match loc_expr with
  | Unit (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"()\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Val (v, _) ->
    (match v with
     | Int (i, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           (string_of_int i)
           line
           start_idx
           end_idx
       , node_name )
     | String (s, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           s
           line
           start_idx
           end_idx
       , node_name )
     | Bool (b, (_, line, start_idx, end_idx)) ->
       ( Printf.sprintf
           "%s [label=\"%s\\nLine: %d\n\
            Starting char index: %d\n\
            Ending char index: %d\"];\n"
           node_name
           (string_of_bool b)
           line
           start_idx
           end_idx
       , node_name ))
  | Var (VarId (id, _), (_, line, start_idx, end_idx)) ->
    ( Printf.sprintf
        "%s [label=\"%s\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        id
        line
        start_idx
        end_idx
    , node_name )
  | UnOp (op, e, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_expr e in
    let un_op_node =
      Printf.sprintf
        "%s [label=\"UnOp\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let c2, n2 = dot_un_op op in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    un_op_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | BinOp (e1, op, e2, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let bin_op_node =
      Printf.sprintf
        "%s [label=\"BinOp\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let c3, n3 = dot_bin_op op in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    let edge3 = Printf.sprintf "%s -> %s;\n" node_name n3 in
    bin_op_node ^ edge1 ^ edge2 ^ edge3 ^ c1 ^ c2 ^ c3, node_name
  | Let (VarId (id, _), _, e1, e2, (_, line, start_idx, end_idx)) ->
    (*the Let is changed to take in the additional type!!!!!!!!!!!!!!!!!!!!!!!!!*)
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let let_node =
      Printf.sprintf
        "%s [label=\"Let: %s\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        id
        line
        start_idx
        end_idx
    in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    let_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Pair (e1, e2, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_expr e1 in
    let c2, n2 = dot_local_expr e2 in
    let pair_node =
      Printf.sprintf
        "%s [label=\"Pair\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    pair_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name
  | Fst (e, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_expr e in
    let fst_node =
      Printf.sprintf
        "%s [label=\"Fst\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    fst_node ^ edge ^ c, node_name
  | Snd (e, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_expr e in
    let snd_node =
      Printf.sprintf
        "%s [label=\"Snd\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    snd_node ^ edge ^ c, node_name
  | Left (e, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_expr e in
    let left_node =
      Printf.sprintf
        "%s [label=\"Left\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    left_node ^ edge ^ c, node_name
  | Right (e, (_, line, start_idx, end_idx)) ->
    let c, n = dot_local_expr e in
    let right_node =
      Printf.sprintf
        "%s [label=\"Right\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let edge = Printf.sprintf "%s -> %s;\n" node_name n in
    right_node ^ edge ^ c, node_name
  | Match (e, cases, (_, line, start_idx, end_idx)) ->
    let c1, n1 = dot_local_expr e in
    let match_node =
      Printf.sprintf
        "%s [label=\"Match\\nLine: %d\n\
         Starting char index: %d\n\
         Ending char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    in
    let c2, n2 = dot_local_cases cases in
    let edge1 = Printf.sprintf "%s -> %s;\n" node_name n1 in
    let edge2 = Printf.sprintf "%s -> %s;\n" node_name n2 in
    match_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name

(** [dot_local_cases cases] calls [dot_local_case] on each local case in [cases] and creates a tuple.

    - [cases] is a list of local cases.
    - Returns: Tuples of strings where the first element is the dot code for the local cases in [cases] and the second element is the
      node name of the local cases in [cases]. *)
and dot_local_cases cases =
  let case_nodes, case_node_names = List.split (List.map dot_local_case cases) in
  String.concat "" case_nodes, String.concat " " case_node_names
(* Because of type matching error / List.iter is a type unit
   List.append also seems to be wrong *)

(** [dot_local_case (patn, expr)] creates the dot code for a local case [(patn, expr)]

    - [(patn, expr)] is a tuple of a local pattern and a local expression.
    - Calls [dot_local_pattern] on the local pattern and [dot_local_expr] on the local expression.
    - Connects the [dot_local_case] node to it's children, the local pattern and local expression nodes.
    - Returns: A tuple of strings where the first element is the dot code for the local case [(patn, expr)]
      and the second element is the node name of the local case [(patn, expr)]. *)
and dot_local_case (patn, expr) =
  let node_name = generate_node_name () in
  let c1, patn_name = dot_local_pattern patn in
  let c2, expr_name = dot_local_expr expr in
  let case_node = Printf.sprintf "%s [label=\"Case\"];\n" node_name in
  let edge1 = Printf.sprintf "%s -> %s;\n" node_name patn_name in
  let edge2 = Printf.sprintf "%s -> %s;\n" node_name expr_name in
  case_node ^ edge1 ^ edge2 ^ c1 ^ c2, node_name

(** [dot_bin_op op] creates the dot code for the binary operator [op]

    - [op] is a binary operator that is either Plus, Minus, Times, Div, And, Or, Eq, Neq, Lt, Leq, Gt, or Geq.
    - Each label for the binary operator is a string representation of the operator.
    - Returns: A tuple of strings where the first element is the dot code for the binary operator [op] and the
      second element is the node name of the binary operator [op]. *)
and dot_bin_op (op : Local.bin_op) : string * string =
  let node_name = generate_node_name () in
  match op with
  | Plus (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"+\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Minus (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"-\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Times (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"*\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Div (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"/\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | And (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"&&\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Or (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"||\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Eq (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"=\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Neq (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"!=\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Lt (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"<\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Leq (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"<=\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Gt (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\">\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Geq (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\">=\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )

and dot_un_op (op : Local.un_op) : string * string =
  let node_name = generate_node_name () in
  match op with
  | Not (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"!\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )
  | Neg (_, line, start_idx, end_idx) ->
    ( Printf.sprintf
        "%s [label=\"¬\\nLine: %d\nStarting char index: %d\nEnding char index: %d\"];\n"
        node_name
        line
        start_idx
        end_idx
    , node_name )

(** [string_of_value v] returns a string representation of the value [v]

    - [v] is a value that is either an integer, string, or boolean.
    - Returns: A string representation of the value [v]. *)
and string_of_value (v : Ast.Local.value) =
  match v with
  | Int (i, _) -> string_of_int i
  | String (s, _) -> s
  | Bool (b, _) -> string_of_bool b
;;
