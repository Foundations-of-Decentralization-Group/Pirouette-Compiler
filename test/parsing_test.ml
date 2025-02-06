open OUnit2
open Parsing
open Lexing

let parse_string input =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "test" };
  Parse.parse_with_error lexbuf

let test_foreign_decl _ =
  let input = {|foreign myFunc : unit -> unit := "external_function";|} in
  let result = parse_string input in
  match result with
  | [Ast_core.Choreo.M.ForeignDecl (var_id, typ, str, _)] ->
    (* Check variable name *)
    assert_equal "myFunc" 
      (match var_id with Ast_core.Local.M.VarId (name, _) -> name);
    (* Check type *)
    assert_equal true
      (match typ with 
       | Ast_core.Choreo.M.TMap (TUnit _, TUnit _, _) -> true 
       | _ -> false);
    (* Check external function name *)
    assert_equal "external_function" str
  | _ -> assert_failure "Expected ForeignDecl, got different AST structure"


  let test_foreign_decl_unit_to_unit _ =
    let input = {|foreign myFunc : unit -> unit := "external_function";|} in
    let result = parse_string input in
    match result with
    | [Ast_core.Choreo.M.ForeignDecl (var_id, typ, str, _)] ->
      assert_equal "myFunc" 
        (match var_id with Ast_core.Local.M.VarId (name, _) -> name);
      assert_equal true
        (match typ with 
         | Ast_core.Choreo.M.TMap (TUnit _, TUnit _, _) -> true 
         | _ -> false);
      assert_equal "external_function" str
    | _ -> assert_failure "Expected ForeignDecl, got different AST structure"
let suite =
  "Parser Tests" >::: [
    "test_foreign_decl" >:: test_foreign_decl;
    "test_foreign_decl_unit_to_unit" >:: test_foreign_decl_unit_to_unit;
  ]

let () = 
  run_test_tt_main suite
