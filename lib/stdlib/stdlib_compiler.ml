module Local = Ast_core.Local.M
module Net = Ast_core.Net.M
module type Msg_intf = Ocamlgen.Msg_intf.M

open Ppxlib

module Builder = Ast_builder.Make (struct
    let loc = { !Ast_helper.default_loc with loc_ghost = true }
  end)




module Id = struct
  let i = ref 0

  let gen name =
    incr i;
    Printf.sprintf "%s%d" name !i
  ;;
end

let loc = Builder.loc

exception Main_expr of expression

let rec emit_local_pexp (expr : 'a Local.expr) =
  match expr with
  | Unit _ -> "Unit ()"
  | Val (Int (i, _), _) -> "Val (Int (" ^ (string_of_int (i)) ^ ", ()), ())"
  | Val (String (s, _), _) -> "Val (String (" ^ s ^ ", ()), ())"
  | Val (Bool (b, _), _) -> "Val (Bool (" ^ (string_of_bool (b)) ^ ", ()), ())"
  | Var (VarId (v, _), _) -> "Val (VarId (" ^ v ^ ", ()), ())"
  | UnOp (Not _, e, _) -> "UnOp (Not (), )" ^ emit_local_pexp e ^ ", ())"
  | UnOp (Neg _, e, _) -> "UnOp (Neg (), )" ^ emit_local_pexp e ^ ", ())"
  | BinOp (e1, op, e2, _) ->
    let op =
      match op with
      | Plus _ -> "Plus ()"
      | Minus _ -> "Minus ()"
      | Times _ -> "Times ()"
      | Div _ -> "Div ()"
      | And _ -> "And ()"
      | Or _ -> "Or ()"
      | Eq _ -> "Eq ()"
      | Neq _ -> "Neq ()"
      | Lt _ -> "Lt ()"
      | Leq _ -> "Leq ()"
      | Gt _ -> "Gt ()"
      | Geq _ -> "Geq ()"
    in
    "BinOp (" ^ (emit_local_pexp e1) ^ ", " ^ op ^ ", " ^ (emit_local_pexp e2) ^ ", ())"
    (* Dropping type info (2nd '_') since the STDLIB and be assumed to be well-typed*)
  | Let (VarId (v, _), _, e1, e2, _) -> "Let (VarId (" ^ v ^ ", ()), (), " ^ emit_local_pexp e1 ^ ", " ^ emit_local_pexp e2 ^ ", ())"
  | Pair (e1, e2, _) -> "Pair (" ^ emit_local_pexp e1 ^ ", " ^ emit_local_pexp e2 ^ ", ())"
  | Fst (e, _) -> "Fst (" ^ emit_local_pexp e ^ ", ())"
  | Snd (e, _) -> "Snd (" ^ emit_local_pexp e ^ ", ())"
  | Left (e, _) -> "Left (" ^ emit_local_pexp e ^ ", ())"
  | Right (e, _) -> "Right (" ^ emit_local_pexp e ^ ", ())"
  | Match (e, cases, _) ->
    let str_cases =
(      List.fold_left
        (fun acc (pattern, expr) -> acc ^ "(" ^ emit_local_ppat pattern ^ ", " ^ emit_local_pexp expr ^ "); ")
         "[" cases)

         ^ "]" 
    in
    "Match (" ^ emit_local_pexp e ^ ", " ^ str_cases ^ ", ())"

and emit_local_ppat (pat : 'a Local.pattern) =
  match pat with
  | Default _ -> "Default ()"
  | Val (Int (i, _), _) -> "Val (Int (" ^ (string_of_int i) ^ ", ()), ())"
  | Val (String (s, _), _) -> "Val (String (" ^ s ^ ", ()), ())"
  | Val (Bool (b, _), _) -> "Val (Bool (" ^ (string_of_bool b) ^ ", ()), ())"
  | Var (VarId (v, _), _) -> "Val (VarId (" ^ v ^ ", ()), ())"
  | Pair (p1, p2, _) -> "Pair (" ^ (emit_local_ppat p1) ^ ", " ^ (emit_local_ppat p2) ^ ", ())"
  | Left (p, _) -> "Left (" ^ (emit_local_ppat p) ^ ", ())"
  | Right (p, _) -> "Right (" ^ (emit_local_ppat p) ^ ", ())"
;;

let rec emit_net_fun_body
          ~(self_id : string)
          (module Msg : Msg_intf)
          (pats : 'a Local.pattern list)
          (exp : 'a Net.expr)
  =
  match pats with
  | [] -> emit_net_pexp ~self_id (module Msg : Msg_intf) exp
  | f :: ps ->
    Builder.pexp_fun
      Nolabel
      None
      (emit_local_ppat f)
      (emit_net_fun_body ~self_id (module Msg) ps exp)

and emit_net_binding ~(self_id : string) (module Msg : Msg_intf) (stmt : 'a Net.stmt) =
  match stmt with
  | Assign (ps, e, _) ->
    (match ps with
     | [] -> failwith "Error: Empty assignment"
     | Var (VarId ("main", _), _) :: _ ->
       raise (Main_expr (emit_net_pexp ~self_id (module Msg) e))
     | Default _ :: _ ->
       Builder.value_binding
         ~pat:(Builder.pvar (Id.gen "_unit_"))
         ~expr:(emit_net_pexp ~self_id (module Msg) e)
     | [ var ] ->
       Builder.value_binding
         ~pat:(emit_local_ppat var)
         ~expr:(emit_net_pexp ~self_id (module Msg) e)
     | f :: ps ->
       Builder.value_binding
         ~pat:(emit_local_ppat f)
         ~expr:(emit_net_fun_body ~self_id (module Msg) ps e))
  | ForeignDecl (VarId (id, _), typ, external_name, _) -> ""
  | _ -> Builder.value_binding ~pat:[%pat? _unit] ~expr:Builder.eunit

and emit_foreign_decl id typ external_name=
  let open Ast_builder.Default in
  let package_name, function_name, _ =
    Ast_utils.parse_external_name external_name
  in
  let package_string =
    match package_name with
    | Some pack -> pack ^ "."
    | None -> ""
  in

  let fun_expr =
    pexp_fun
      ~loc
      Nolabel
      None
      (pvar ~loc ("="))
      [%expr
        [%e evar ~loc (package_string ^ function_name)]
        [%e evar ~loc "arg"]]
  in
  value_binding ~loc ~pat:(pvar ~loc id) ~expr:fun_expr

and emit_net_pexp ~(self_id : string) (module Msg : Msg_intf) (exp : 'a Net.expr) =
  match exp with
  | Unit _ -> Builder.eunit
  | Var (VarId (v, _), _) -> Builder.evar v
  | Ret (e, _) -> emit_local_pexp e
  | If (e1, e2, e3, _) ->
    Builder.pexp_ifthenelse
      (emit_net_pexp ~self_id (module Msg) e1)
      (emit_net_pexp ~self_id (module Msg) e2)
      (Some (emit_net_pexp ~self_id (module Msg) e3))
  | Let (stmts, e, _) ->
    Builder.pexp_let
      Recursive (*FIXME: how to handle tuples?*)
      (List.map (emit_net_binding ~self_id (module Msg)) stmts)
      (emit_net_pexp ~self_id (module Msg) e)
  | FunDef (ps, e, _) -> emit_net_fun_body ~self_id (module Msg) ps e
  | FunApp (e1, e2, _) ->
    [%expr
      [%e emit_net_pexp ~self_id (module Msg) e1]
        [%e emit_net_pexp ~self_id (module Msg) e2]]
  | Pair (e1, e2, _) ->
    [%expr
      [%e emit_net_pexp ~self_id (module Msg) e1]
    , [%e emit_net_pexp ~self_id (module Msg) e2]]
  | Fst (e, _) -> [%expr fst [%e emit_net_pexp ~self_id (module Msg) e]]
  | Snd (e, _) -> [%expr snd [%e emit_net_pexp ~self_id (module Msg) e]]
  | Left (e, _) -> [%expr Either.Left [%e emit_net_pexp ~self_id (module Msg) e]]
  | Right (e, _) -> [%expr Either.Right [%e emit_net_pexp ~self_id (module Msg) e]]
  | Match (e, cases, _) ->
    let cases =
      List.map
        (fun (p, e) ->
           Builder.case
             ~lhs:(emit_local_ppat p)
             ~guard:None
             ~rhs:(emit_net_pexp ~self_id (module Msg) e))
        cases
    in
    Builder.pexp_match (emit_net_pexp ~self_id (module Msg) e) cases
  | Send (e, LocId (dst, _), _) ->
    let val_id = Id.gen "val_" in
    [%expr
      let [%p Builder.pvar val_id] = [%e emit_net_pexp ~self_id (module Msg) e] in
      [%e
        Msg.emit_net_send
          ~src:self_id
          ~dst
          [%expr Marshal.to_string [%e Builder.evar val_id] []]]]
  | Recv (LocId (src, _), _) ->
    [%expr Marshal.from_string [%e Msg.emit_net_recv ~src ~dst:self_id] 0]
  | ChooseFor (LabelId (label, _), LocId (dst, _), e, _) ->
    Builder.esequence
      [ Msg.emit_net_send ~src:self_id ~dst (Builder.estring label)
      ; emit_net_pexp ~self_id (module Msg) e
      ]
  | AllowChoice (LocId (src, _), cases, _) ->
    let cases =
      List.map
        (fun (Local.LabelId (label, _), e) ->
           Builder.case
             ~lhs:(Builder.pstring label)
             ~guard:None
             ~rhs:(emit_net_pexp ~self_id (module Msg) e))
        cases
    and default_case =
      Builder.case
        ~lhs:Builder.ppat_any
        ~guard:None
        ~rhs:[%expr failwith "Runtime Error: Unmatched label"]
    in
    Builder.pexp_match (Msg.emit_net_recv ~src ~dst:self_id) (cases @ [ default_case ])
;;



let loc = Builder.loc
let spf = Printf.sprintf

module Msg_chan_intf : Msg_intf = struct
  let emit_net_send ~src ~dst pexp =
    [%expr Domainslib.Chan.send [%e Builder.evar (spf "chan_%s_%s" src dst)] [%e pexp]]
  ;;

  let emit_net_recv ~src ~dst =
    [%expr Domainslib.Chan.recv [%e Builder.evar (spf "chan_%s_%s" src dst)]]
  ;;
end

let emit_toplevel_domain
      out_chan
      (loc_ids : string list)
      (net_stmtblocks : 'a Net.stmt_block list)
  =
  let emit_domain_bindings loc_ids net_stmtblocks : value_binding list =
    (* convert a list of statements into a let-in chain *)
    let main_expr = ref Builder.eunit in
    let rec emit_net_toplevel loc_id stmts : expression =
      match stmts with
      | [] -> !main_expr
      | stmt :: stmts ->
        (match emit_net_binding ~self_id:loc_id (module Msg_chan_intf) stmt with
         | exception Main_expr e ->
           main_expr := e;
           emit_net_toplevel loc_id stmts
         | binding ->
           Builder.pexp_let Recursive [ binding ] (emit_net_toplevel loc_id stmts))
    in
    List.map2
      (fun loc_id net_stmtblock ->
         Builder.value_binding
           ~pat:(Builder.pvar (spf "domain_%s" loc_id))
           ~expr:
             [%expr Domain.spawn (fun _ -> [%e emit_net_toplevel loc_id net_stmtblock])])
      loc_ids
      net_stmtblocks
  in
  let rec emit_domain_join_seq : string list -> expression = function
    | [] -> assert false
    | [ loc_id ] -> [%expr Domain.join [%e Builder.evar (spf "domain_%s" loc_id)]]
    | loc_id :: loc_ids ->
      Builder.pexp_sequence
        [%expr Domain.join [%e Builder.evar (spf "domain_%s" loc_id)]]
        (emit_domain_join_seq loc_ids)
  in
  let emit_chan_defs loc_ids : structure_item list =
    let loc_pairs =
      List.concat_map
        (fun a -> List.filter_map (fun b -> if a <> b then Some (a, b) else None) loc_ids)
        loc_ids
    in
    List.map
      (fun (a, b) ->
         [%stri
           let [%p Builder.pvar (spf "chan_%s_%s" a b)] : string Domainslib.Chan.t =
             Domainslib.Chan.make_bounded 0
           ;;])
      loc_pairs
  in
  Printf.fprintf out_chan "%s\n" {|[@@@warning "-26"]|};
  Printf.fprintf out_chan "%s" "let ast = ";

  let ppf = Format.formatter_of_out_channel out_chan in
  Pprintast.structure
    ppf
    (emit_chan_defs loc_ids
     @ [ Builder.pstr_eval
           (Builder.pexp_let
              Nonrecursive
              (emit_domain_bindings loc_ids net_stmtblocks)
              (emit_domain_join_seq loc_ids))
           []
       ]);
  Format.pp_print_newline ppf ()
;;


(* Check if there exists a PIR_STDLIB environment variable on the user's system. If not, print on stderr and exit *)
let compile_stdlib () : unit =
  let (path_to_stdlib:string) = match Sys.getenv_opt "PIR_STDLIB" with
    | Some p ->
        p
    | None ->
        prerr_endline ("Path to Pirouette standard library not set in enviroment variables as \"PIR_STDLIB\"=[ABSOLUTE_PATH_TO_YOUR_STDLIB]\n"); exit 1
  in

  (* Create an OCaml file representation -via opening the file for reading as an in channel- of the path to the standard library file *)
  let file_ic_stlid = open_in path_to_stdlib in

  (* Lex the file *)
  let lexbuf_stdlib = Lexing.from_channel file_ic_stlid in
    (* Return the AST created from the parsed lex *)
  let stdlib_ast = A_rname.Rename.ast_list_alpha_rename (Parsing.Parse.parse_with_error (path_to_stdlib) (lexbuf_stdlib)) in

  let stdlib_locs = Ast_utils.extract_locs stdlib_ast in

  let stdlib_netir_l = List.map (fun loc -> Netgen.epp_choreo_to_net stdlib_ast loc) stdlib_locs in

  (* There must be a stdlib_ast.ml file in the same directory as your stdlib.pir pointed to by your PIR_STDLIB env var*)
  let stdlib_out_path = (Filename.dirname path_to_stdlib)^Filename.dir_sep^"stdlib.gen.ml" in

  (* emit_toplevel_domain (open_out stdlib_out_path) stdlib_locs stdlib_netir_l;; *)

  let stdlib_out_channel = Out_channel.open_bin stdlib_out_path in
  let ppf = Format.formatter_of_out_channel stdlib_out_channel in
  Pprintast.expression ppf stdlib_netir_l; Out_channel;;
;;