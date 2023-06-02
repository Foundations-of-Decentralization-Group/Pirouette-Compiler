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
  let input = "(fun funname (X) := Seller.b ~> Buyer.l; X) s.g" in
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
  let input = "fun funname (X) := Seller.b ~> Buyer.l; Buyer[R] ~> Seller; X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Fun {name = "funname"; arg = (Expr.ChoreoVars "X"); 
      body = Expr.Let {fst = Expr.Assoc {loc = "Buyer"; arg = (Expr.Variable "l")}; 
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Seller"; arg = (Expr.Variable "b")};
      name = "Buyer"}; thn = (Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller"; thn = (Expr.ChoreoVars "X")})};}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_sync_success _ =
  let input = "Buyer[R] ~> Seller; X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller"; thn = (Expr.ChoreoVars "X")}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_if_thn_else_success _ =
  let input = "if l.e then Buyer[L] ~> Seller; X else Buyer[R] ~> Seller; X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Branch {ift = Expr.Assoc {loc = "l"; arg = (Expr.Variable "e")};
        thn = Expr.Sync {sndr = "Buyer"; d = "L"; rcvr = "Seller"; thn = (Expr.ChoreoVars "X")};        
        el = Expr.Sync {sndr = "Buyer"; d = "R"; rcvr = "Seller"; thn = (Expr.ChoreoVars "X")}}  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_map_equation_success _ =
  let input = "Buyer.s ~> Seller.p; Seller.prices[b] ~> Buyer.c; if Buyer.(c / 2 < price) then X else Y" in
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

let test_parse_let_in1 _ =
  let input = "let l.x := l.5 in l.x" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Let {fst = Expr.Assoc {loc = "l"; arg = (Expr.Variable "x")}; 
      snd = Expr.Snd { sndr = Expr.Assoc {loc = "l"; arg = (Expr.Value 5)}; name = "l"}; 
      thn = Expr.Assoc {loc = "l"; arg = (Expr.Variable "x")};}
    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_let_in2 _ =
  let input = "let l.x := l.5 in l.(x + 3)" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Let {fst = Expr.Assoc {loc = "l"; arg = (Expr.Variable "x")}; 
      snd = Expr.Snd { sndr = Expr.Assoc {loc = "l"; arg = (Expr.Value 5)}; name = "l"}; 
      thn = Expr.Assoc {loc = "l"; arg = Expr.Plus {lft = (Expr.Variable "x"); rght = (Expr.Value 3)}};}
    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_complex_success _ =
  let input = "Person1.amt_due ~> Person2.d; 
  (fun initpay (X) :=
  if Person2.(x < 500)
  then Person2[L] ~> Person1; 
  Person1.rem ~> Person2.rcv; 
  Person1.(amt_due - rem) ~> Person2.y; 
  Person2.d 
  else Person2[R] ~> Person1; 
  Person1.500 ~> Person2.rcv; 
  Person1.0 ~> Person2.y; 
  X) Person2.d" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")};
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
              Expr.Condition {lft = (Expr.Variable "x"); op = "<";
                rght = (Expr.Value 500)}};
            thn =
            Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1";
              thn =
              Expr.Let {
                fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
                snd =
                Expr.Snd {
                  sndr =
                  Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "rem")};
                  name = "Person2"};
                thn =
                Expr.Let {
                  fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")};
                  snd =
                  Expr.Snd {
                    sndr =
                    Expr.Assoc {loc = "Person1";
                      arg =
                      Expr.Minus {lft = (Expr.Variable "amt_due");
                        rght = (Expr.Variable "rem")}};
                    name = "Person2"};
                  thn = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}};
            el =
            Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1";
              thn =
              Expr.Let {
                fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
                snd =
                Expr.Snd {
                  sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 500)};
                  name = "Person2"};
                thn =
                Expr.Let {
                  fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")};
                  snd =
                  Expr.Snd {
                    sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)};
                    name = "Person2"};
                  thn = (Expr.ChoreoVars "X")}}}}};
        argument = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}
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
    "test_parse_let_in1" >:: test_parse_let_in1;
    "test_parse_let_in2" >:: test_parse_let_in2;
    "test_parse_complex_success" >:: test_parse_complex_success;
    (* Add more test cases here *)
  ]

let () = run_test_tt_main ("parser_tests" >::: [suite]);