open OUnit2
open Lexing
open Expr

(* SUCCESS Test Cases *)
let test_parse_le_success _ =
  let input = "person1.(x : int)" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Assoc ((Location "person1"),
      (Variable ((Name "x"), (Some IntType))), None))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

  
let test_parse_send_success _ =
  let input = "person1.(x : int) ~> person2.(y : int); X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected =
        (Expr.Let ((Expr.Location "person2"),
   (Expr.Variable ((Expr.Name "y"), (Some Expr.IntType))),
   (Expr.Snd (
      (Expr.Assoc ((Expr.Location "person1"),
         (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))), None)),
      (Expr.Location "person2"), None)),
   (Expr.ChoreoVars ((Expr.Name "X"), None)), None)) in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_application_X_success _ =
    let input = "(fun init 
    (X : person1.int) : person1.int
    := person1.(x : int) ~> person2.(y : int); 
    X) person1.(3+2)" in
    let lexer = from_string input in
    match Parser.prog Lexer.read lexer with
    | Some expr ->
        let expected = (Expr.Application (                 
          (Expr.FunG ((Expr.Name "init"),
             (Expr.ChoreoVars ((Expr.Name "X"),
                (Some (Expr.DotType ((Expr.Location "person1"), Expr.IntType))))),
             (Expr.Let ((Expr.Location "person2"),
                (Expr.Variable ((Expr.Name "y"), (Some Expr.IntType))),
                (Expr.Snd (
                   (Expr.Assoc ((Expr.Location "person1"),
                      (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))), None)),
                   (Expr.Location "person2"), None)),
                (Expr.ChoreoVars ((Expr.Name "X"), None)), None)),
             (Some (Expr.ArrowType (
                      (Expr.DotType ((Expr.Location "person1"), Expr.IntType)),
                      (Expr.DotType ((Expr.Location "person1"), Expr.IntType)))))
             )),
          (Expr.Assoc ((Expr.Location "person1"),
             (Expr.Plus ((Expr.INT 3), (Expr.INT 2), (Some Expr.IntType))), None)),
          None))  in
        assert_equal expected expr
    | None -> assert_failure "Failed to parse expression"

let test_parse_application_local_success _ =
  let input = "(fun init 
(person2.(name : string)) : person1.int
:= person1.(x : int) ~> person2.(y : int); 
person2.name) person2.\"hello\"" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Application (                 
        (Expr.FunL ((Expr.Name "init"), (Expr.Location "person2"),
           (Expr.Variable ((Expr.Name "name"), (Some Expr.StringType))),
           (Expr.Let ((Expr.Location "person2"),
              (Expr.Variable ((Expr.Name "y"), (Some Expr.IntType))),
              (Expr.Snd (
                 (Expr.Assoc ((Expr.Location "person1"),
                    (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))), None)),
                 (Expr.Location "person2"), None)),
              (Expr.Assoc ((Expr.Location "person2"),
                 (Expr.Variable ((Expr.Name "name"), None)), None)),
              None)),
           (Some (Expr.ArrowType (
                    (Expr.DotType ((Expr.Location "person2"), Expr.StringType)),
                    (Expr.DotType ((Expr.Location "person1"), Expr.IntType)))))
           )),
        (Expr.Assoc ((Expr.Location "person2"), (Expr.STRING "hello"), None)),
        None))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_choreo_var_arrowtype_success _ =
  let input = "X : Person1.int -> person2.string" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.ChoreoVars ((Expr.Name "X"),  
      (Some (Expr.ArrowType (
               (Expr.DotType ((Expr.Location "Person1"), Expr.IntType)),
               (Expr.DotType ((Expr.Location "person2"), Expr.StringType)))))
      ))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_fun_expr_success _ =
  let input = "fun init 
  (X : person1.int)
  := person1.(x : int) ~> person2.(y : int); 
  X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.FunG ((Expr.Name "init"),     
      (Expr.ChoreoVars ((Expr.Name "X"),
         (Some (Expr.DotType ((Expr.Location "person1"), Expr.IntType))))),
      (Expr.Let ((Expr.Location "person2"),
         (Expr.Variable ((Expr.Name "y"), (Some Expr.IntType))),
         (Expr.Snd (
            (Expr.Assoc ((Expr.Location "person1"),
               (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))), None)),
            (Expr.Location "person2"), None)),
         (Expr.ChoreoVars ((Expr.Name "X"), None)), None)),
      None))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_sync_success _ =
  let input = "Buyer[R] ~> Seller; X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Sync ((Expr.Location "Buyer"), (Expr.Direction "R"),
   (Expr.Location "Seller"), (Expr.ChoreoVars ((Expr.Name "X"), None)), None
   ))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_if_thn_else_success _ =
  let input = "if l.(4<5) 
    then 
    Buyer[L] ~> Seller; 
    X : person1.int 
    else 
    Buyer[R] ~> Seller; 
    X : person1.int" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Branch (                      
        (Expr.Assoc ((Expr.Location "l"),
           (Expr.Condition ((Expr.INT 4), Expr.Lt, (Expr.INT 5),
              (Some Expr.BoolType))),
           None)),
        (Expr.Sync ((Expr.Location "Buyer"), (Expr.Direction "L"),
           (Expr.Location "Seller"),
           (Expr.ChoreoVars ((Expr.Name "X"),
              (Some (Expr.DotType ((Expr.Location "person1"), Expr.IntType))))),
           None)),
        (Expr.Sync ((Expr.Location "Buyer"), (Expr.Direction "R"),
           (Expr.Location "Seller"),
           (Expr.ChoreoVars ((Expr.Name "X"),
              (Some (Expr.DotType ((Expr.Location "person1"), Expr.IntType))))),
           None)),
        None)) in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_let_in1 _ =
  let input = "let l.(x : int) := l.5 in l.x" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Let ((Expr.Location "l"),     
   (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))),
   (Expr.Assoc ((Expr.Location "l"), (Expr.INT 5), None)),
   (Expr.Assoc ((Expr.Location "l"), (Expr.Variable ((Expr.Name "x"), None)),
      None)),
   None))

    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_let_in2 _ =
  let input = "let l.(x : int) := l.5 in l.(x + 3)" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Let ((Expr.Location "l"),     
      (Expr.Variable ((Expr.Name "x"), (Some Expr.IntType))),
      (Expr.Assoc ((Expr.Location "l"), (Expr.INT 5), None)),
      (Expr.Assoc ((Expr.Location "l"),
         (Expr.Plus ((Expr.Variable ((Expr.Name "x"), None)), (Expr.INT 3),
            (Some Expr.IntType))),
         None)),
      None)) in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_application_letin _ =
  let input = " (fun fname (X : p.int) := let p.(n : int) := X in p.(n + 1))
  let p.(m : int) := p.3 in p.(m - 1)" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Expr.Application (                 
        (Expr.FunG ((Expr.Name "fname"),
           (Expr.ChoreoVars ((Expr.Name "X"),
              (Some (Expr.DotType ((Expr.Location "p"), Expr.IntType))))),
           (Expr.Let ((Expr.Location "p"),
              (Expr.Variable ((Expr.Name "n"), (Some Expr.IntType))),
              (Expr.ChoreoVars ((Expr.Name "X"), None)),
              (Expr.Assoc ((Expr.Location "p"),
                 (Expr.Plus ((Expr.Variable ((Expr.Name "n"), None)),
                    (Expr.INT 1), (Some Expr.IntType))),
                 None)),
              None)),
           None)),
        (Expr.Let ((Expr.Location "p"),
           (Expr.Variable ((Expr.Name "m"), (Some Expr.IntType))),
           (Expr.Assoc ((Expr.Location "p"), (Expr.INT 3), None)),
           (Expr.Assoc ((Expr.Location "p"),
              (Expr.Minus ((Expr.Variable ((Expr.Name "m"), None)), (Expr.INT 1),
                 (Some Expr.IntType))),
              None)),
           None)),
        None))
    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  
(*
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
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "amt_due")}; name = "Person2"};
      thn = Expr.Application { funct = Expr.Fun {name = "initpay"; arg = (Expr.ChoreoVars "X");
      body = Expr.Branch { ift = Expr.Assoc {loc = "Person2"; arg = Expr.Condition {lft = (Expr.Variable "x"); op = "<"; rght = (Expr.Value 500)}};
      thn = Expr.Sync {sndr = "Person2"; d = "L"; rcvr = "Person1"; thn = Expr.Let { fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
      snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Variable "rem")}; name = "Person2"};
      thn = Expr.Let { fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")};
      snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = Expr.Minus {lft = (Expr.Variable "amt_due");  rght = (Expr.Variable "rem")}}; 
      name = "Person2"}; thn = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}};
      el = Expr.Sync {sndr = "Person2"; d = "R"; rcvr = "Person1"; thn = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "rcv")};
      snd = Expr.Snd { sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 500)}; name = "Person2"};
      thn = Expr.Let {fst = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "y")}; 
      snd = Expr.Snd {sndr = Expr.Assoc {loc = "Person1"; arg = (Expr.Value 0)}; name = "Person2"};thn = (Expr.ChoreoVars "X")}}}}};
      argument = Expr.Assoc {loc = "Person2"; arg = (Expr.Variable "d")}}}
    in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"   *)


let suite =
  "parser_tests" >::: [
    "test_parse_le_success" >:: test_parse_le_success;
    "test_parse_send_success" >:: test_parse_send_success;
    "test_parse_fun_expr_success" >:: test_parse_fun_expr_success; 
    "test_parse_choreo_var_arrowtype_success" >:: test_parse_choreo_var_arrowtype_success; 
    "test_parse_application_X_success" >:: test_parse_application_X_success;
    "test_parse_application_local_success" >:: test_parse_application_local_success;   
    "test_parse_sync_success" >:: test_parse_sync_success;
    "test_parse_if_thn_else_success" >:: test_parse_if_thn_else_success;
    "test_parse_let_in1" >:: test_parse_let_in1;
    "test_parse_let_in2" >:: test_parse_let_in2;
    "test_parse_application_letin" >:: test_parse_application_letin;
    (* "test_parse_complex_success" >:: test_parse_complex_success; *)
    (* Add more test cases here *)
  ]

let () = run_test_tt_main ("parser_tests" >::: [suite]);