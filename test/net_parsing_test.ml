open OUnit2
open Ast_core
open Parsing

(* Helper function to parse a string into a Net AST *)
let parse_net_string str =
  let lexbuf = Lexing.from_string str in
  try
    Net_parser.prog Net_lexer.read lexbuf
  with
  | Net_lexer.SyntaxError msg ->
      failwith (Printf.sprintf "Lexer error: %s" msg)
  | Net_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      failwith (Printf.sprintf "Parser error at line %d, column %d" 
                pos.Lexing.pos_lnum 
                (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))

(* Test suite *)
let net_parsing_tests = "Net Parsing Tests" >::: [
  "test_parse_empty_program" >:: (fun _ ->
    let result = parse_net_string "" in
    assert_equal [] result
  );

  "test_parse_unit_declaration" >:: (fun _ ->
    let result = parse_net_string "x : unit;" in
    match result with
    | [Net.M.Decl(Local.M.Var(Local.M.VarId("x", _), _), Net.M.TUnit _, _)] -> ()
    | _ -> assert_failure "Failed to parse unit declaration"
  );

  "test_parse_basic_assignment" >:: (fun _ ->
    let result = parse_net_string "x := unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], Net.M.Unit _, _)] -> ()
    | _ -> assert_failure "Failed to parse basic assignment"
  );

  "test_parse_type_declaration" >:: (fun _ ->
    let result = parse_net_string "type myType := unit;" in
    match result with
    | [Net.M.TypeDecl(Local.M.TypId("myType", _), Net.M.TUnit _, _)] -> ()
    | _ -> assert_failure "Failed to parse type declaration"
  );

  "test_parse_foreign_declaration" >:: (fun _ ->
    let result = parse_net_string "foreign print : unit := \"console.log\";" in
    match result with
    | [Net.M.ForeignDecl(Local.M.VarId("print", _), Net.M.TUnit _, "console.log", _)] -> ()
    | _ -> assert_failure "Failed to parse foreign declaration"
  );

  "test_parse_let_expression" >:: (fun _ ->
    let result = parse_net_string "x := let y : unit; in unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.Let([Net.M.Decl(Local.M.Var(Local.M.VarId("y", _), _), Net.M.TUnit _, _)], 
                            Net.M.Unit _, _), _)] -> ()
    | _ -> assert_failure "Failed to parse let expression"
  );

  "test_parse_if_expression" >:: (fun _ ->
    let result = parse_net_string "x := if unit then unit else unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.If(Net.M.Unit _, Net.M.Unit _, Net.M.Unit _, _), _)] -> ()
    | _ -> assert_failure "Failed to parse if expression"
  );

  "test_parse_send_recv" >:: (fun _ ->
    let result = parse_net_string "x := send unit ~> server; y := recv from client;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.Send(Net.M.Unit _, Local.M.LocId("server", _), _), _); 
       Net.M.Assign([Local.M.Var(Local.M.VarId("y", _), _)], 
                   Net.M.Recv(Local.M.LocId("client", _), _), _)] -> ()
    | _ -> assert_failure "Failed to parse send/recv expressions"
  );

  "test_parse_function_definition" >:: (fun _ ->
    let result = parse_net_string "f := fun x -> unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("f", _), _)], 
                   Net.M.FunDef([Local.M.Var(Local.M.VarId("x", _), _)], Net.M.Unit _, _), _)] -> ()
    | _ -> assert_failure "Failed to parse function definition"
  );

  "test_parse_pairs" >:: (fun _ ->
    let result = parse_net_string "p := (unit, unit); x := fst p; y := snd p;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("p", _), _)], 
                   Net.M.Pair(Net.M.Unit _, Net.M.Unit _, _), _);
       Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.Fst(Net.M.Var(Local.M.VarId("p", _), _), _), _);
       Net.M.Assign([Local.M.Var(Local.M.VarId("y", _), _)], 
                   Net.M.Snd(Net.M.Var(Local.M.VarId("p", _), _), _), _)] -> ()
    | _ -> assert_failure "Failed to parse pairs and accessors"
  );

  "test_parse_sum_types" >:: (fun _ ->
    let result = parse_net_string "l := left unit; r := right unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("l", _), _)], 
                   Net.M.Left(Net.M.Unit _, _), _);
       Net.M.Assign([Local.M.Var(Local.M.VarId("r", _), _)], 
                   Net.M.Right(Net.M.Unit _, _), _)] -> ()
    | _ -> assert_failure "Failed to parse sum type constructors"
  );

  "test_parse_match_expression" >:: (fun _ ->
    let result = parse_net_string "x := match y with | left z -> unit | right w -> unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.Match(Net.M.Var(Local.M.VarId("y", _), _), 
                              [(Local.M.Left(Local.M.Var(Local.M.VarId("z", _), _), _), Net.M.Unit _);
                               (Local.M.Right(Local.M.Var(Local.M.VarId("w", _), _), _), Net.M.Unit _)], _), _)] -> ()
    | _ -> assert_failure "Failed to parse match expression"
  );

  "test_parse_choose_allow" >:: (fun _ ->
    let result = parse_net_string "x := choose option for client in unit; y := allow choice from server with | option -> unit;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                   Net.M.ChooseFor(Local.M.LabelId("option", _), Local.M.LocId("client", _), Net.M.Unit _, _), _);
       Net.M.Assign([Local.M.Var(Local.M.VarId("y", _), _)], 
                   Net.M.AllowChoice(Local.M.LocId("server", _), 
                                   [(Local.M.LabelId("option", _), Net.M.Unit _)], _), _)] -> ()
    | _ -> assert_failure "Failed to parse choose/allow expressions"
  );

  "test_parse_complex_types" >:: (fun _ ->
    let result = parse_net_string "x : client.int; y : unit -> unit; z : unit * unit; w : unit + unit;" in
    match result with
    | [Net.M.Decl(Local.M.Var(Local.M.VarId("x", _), _), Net.M.TLoc(Local.M.TInt _, _), _);
       Net.M.Decl(Local.M.Var(Local.M.VarId("y", _), _), Net.M.TMap(Net.M.TUnit _, Net.M.TUnit _, _), _);
       Net.M.Decl(Local.M.Var(Local.M.VarId("z", _), _), Net.M.TProd(Net.M.TUnit _, Net.M.TUnit _, _), _);
       Net.M.Decl(Local.M.Var(Local.M.VarId("w", _), _), Net.M.TSum(Net.M.TUnit _, Net.M.TUnit _, _), _)] -> ()
    | _ -> assert_failure "Failed to parse complex types"
  );

  "test_parse_ret_expression" >:: (fun _ ->
    let result = parse_net_string "x := ret 42;" in
    match result with
    | [Net.M.Assign([Local.M.Var(Local.M.VarId("x", _), _)], 
                  Net.M.Ret(Local.M.Val(Local.M.Int(42, _), _), _), _)] -> ()
    | _ -> assert_failure "Failed to parse ret expression"
  );

  "test_parse_comments" >:: (fun _ ->
    let result = parse_net_string "-- This is a comment\nx : unit; {- This is a\nmulti-line comment -} y : unit;" in
    match result with
    | [Net.M.Decl(Local.M.Var(Local.M.VarId("x", _), _), Net.M.TUnit _, _);
       Net.M.Decl(Local.M.Var(Local.M.VarId("y", _), _), Net.M.TUnit _, _)] -> ()
    | _ -> assert_failure "Failed to parse program with comments"
  );
]

(* Register the tests *)
let () =
  run_test_tt_main net_parsing_tests
