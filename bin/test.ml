open OUnit2
open Lexing
open Expr

(* SUCCESS Test Cases *)
let test_parse_expr_success _ =
  let input = "l.e" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Assoc {loc = "l"; arg = (Expr.Variable "e")}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_function_application_with_brackets_success _ =
    let input = "(fun funname (X) := X) s.g" in
    let lexer = from_string input in
    match Parser.prog Lexer.read lexer with
    | Some expr ->
        let expected = Expr.Application {
          funct =
          Expr.Fun {name = "funname"; arg = (Expr.ChoreoVars "X");
            body = (Expr.ChoreoVars "X")};
          argument = Expr.Assoc {loc = "s"; arg = (Expr.Variable "g")}}  in
        assert_equal expected expr
    | None -> assert_failure "Failed to parse expression"

let test_parse_function_application_with_choreo_body_success _ =
  let input = "(fun funname (X) := Seller.b @> Buyer.l; X) s.g" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Application {
        funct =
        Expr.Fun {name = "funname"; arg = (Expr.ChoreoVars "X");
          body = Expr.Let {fst = Expr.Assoc {loc = "Buyer"; arg = (Expr.Variable "l")}; 
          snd = Expr.Snd {sndr = Expr.Assoc {loc = "Seller"; arg = (Expr.Variable "b")};
          name = "Buyer"}; thn = (Expr.ChoreoVars "X")}};
        argument = Expr.Assoc {loc = "s"; arg = (Expr.Variable "g")}}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_choreo_var_success _ =
  let input = "X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.ChoreoVars "X"  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_fun_expr_success _ =
  let input = "fun funname (X) := Seller.b @> Buyer.l; Buyer[R] @> Seller;" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Fun {name = "funname"; arg = (Expr.ChoreoVars "X"); 
      body = Expr.Let {fst = Expr.Assoc {loc = "Buyer"; arg = (Expr.Variable "l")}; 
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Seller"; arg = (Expr.Variable "b")};
      name = "Buyer"}; thn = (Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller";})};}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_sync_success _ =
  let input = "Buyer[R] @> Seller;" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller";}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_if_thn_else_success _ =
  let input = "if l.e then Buyer[L] @> Seller; else Buyer[R] @> Seller;" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Branch {ift = Expr.Assoc {loc = "l"; arg = (Expr.Variable "e")};
        thn = Expr.Sync {sndr = "Buyer"; d = "L"; rcvr = "Seller"};        
        el = Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller"}}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_map_equation_success _ =
  let input = "Buyer.s @> Seller.p; Seller.prices[b] @> Buyer.c; if Buyer.(c / 2 < price) then X else Y" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Let {fst = Expr.Assoc {loc = "Seller"; arg = (Expr.Variable "p")};
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Buyer"; arg = (Expr.Variable "s")}; name = "Seller"};
      thn = Expr.Let {fst = Expr.Assoc {loc = "Buyer"; arg = (Expr.Variable "c")};
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Seller"; arg = Expr.Map {name = "prices"; arg = (Expr.Variable "b")}};
      name = "Buyer"}; thn = Expr.Branch { ift = Expr.Assoc {loc = "Buyer";
      arg = Expr.Condition { lft = Expr.Division {lft = (Expr.Variable "c"); rght = (Expr.Value 2)};
      op = "<"; rght = (Expr.Variable "price")}}; thn = (Expr.ChoreoVars "X"); el = (Expr.ChoreoVars "Y")}}}  
    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let suite =
  "parser_tests" >::: [
    "test_parse_expr_success" >:: test_parse_expr_success;
    "test_parse_function_application_with_brackets_success" >:: test_parse_function_application_with_brackets_success;
    "test_parse_function_application_with_choreo_body_success" >:: test_parse_function_application_with_choreo_body_success;  
    "test_parse_choreo_var_success" >:: test_parse_choreo_var_success; 
    "test_parse_fun_expr_success" >:: test_parse_fun_expr_success;  
    "test_parse_sync_success" >:: test_parse_sync_success;
    "test_parse_if_thn_else_success" >:: test_parse_if_thn_else_success;
    "test_parse_map_equation_success" >:: test_parse_map_equation_success;
    (* Add more test cases here *)
  ]

let () = run_test_tt_main ("parser_tests" >::: [suite]);