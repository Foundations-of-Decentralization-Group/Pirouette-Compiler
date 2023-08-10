open OUnit2
open Lexing
open Pirouette.Expr
open Basictypes

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
        (Let ((Location "person2"),
   (Variable ((Name "y"), (Some IntType))),
   (Snd (
      (Assoc ((Location "person1"),
         (Variable ((Name "x"), (Some IntType))), None)),
      (Location "person2"), None)),
   (ChoreoVars ((Name "X"), None)), None)) in
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
        let expected = (Application (                 
          (Fun ((Name "init"),
             (ChoreoVars ((Name "X"),
                (Some (DotType ((Location "person1"), IntType))))),
             (Let ((Location "person2"),
                (Variable ((Name "y"), (Some IntType))),
                (Snd (
                   (Assoc ((Location "person1"),
                      (Variable ((Name "x"), (Some IntType))), None)),
                   (Location "person2"), None)),
                (ChoreoVars ((Name "X"), None)), None)),
             (Some (ArrowType (
                      (DotType ((Location "person1"), IntType)),
                      (DotType ((Location "person1"), IntType)))))
             )),
          (Assoc ((Location "person1"),
             (Plus ((INT 3), (INT 2), (Some IntType))), None)),
          None))  in
        assert_equal expected expr
    | None -> assert_failure "Failed to parse expression"

(* let test_parse_application_local_success _ =
  let input = "(fun init 
(person2.(name : string)) : person1.int
:= person1.(x : int) ~> person2.(y : int); 
person2.name) person2.\"hello\"" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Application (                 
        (FunL ((Name "init"), (Location "person2"),
           (Variable ((Name "name"), (Some StringType))),
           (Let ((Location "person2"),
              (Variable ((Name "y"), (Some IntType))),
              (Snd (
                 (Assoc ((Location "person1"),
                    (Variable ((Name "x"), (Some IntType))), None)),
                 (Location "person2"), None)),
              (Assoc ((Location "person2"),
                 (Variable ((Name "name"), None)), None)),
              None)),
           (Some (ArrowType (
                    (DotType ((Location "person2"), StringType)),
                    (DotType ((Location "person1"), IntType)))))
           )),
        (Assoc ((Location "person2"), (STRING "hello"), None)),
        None))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression" *)

let test_parse_choreo_var_arrowtype_success _ =
  let input = "X : Person1.int -> person2.string" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (ChoreoVars ((Name "X"),  
      (Some (ArrowType (
               (DotType ((Location "Person1"), IntType)),
               (DotType ((Location "person2"), StringType)))))
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
      let expected = (Fun ((Name "init"),     
      (ChoreoVars ((Name "X"),
         (Some (DotType ((Location "person1"), IntType))))),
      (Let ((Location "person2"),
         (Variable ((Name "y"), (Some IntType))),
         (Snd (
            (Assoc ((Location "person1"),
               (Variable ((Name "x"), (Some IntType))), None)),
            (Location "person2"), None)),
         (ChoreoVars ((Name "X"), None)), None)),
      None))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_sync_success _ =
  let input = "Buyer[R] ~> Seller; X" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Sync ((Location "Buyer"), (Direction "R"),
   (Location "Seller"), (ChoreoVars ((Name "X"), None)), None
   ))  in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"

let test_parse_if_thn_else_success _ =
  let input = "if Buyer.(4<5) 
    then 
    Buyer[L] ~> Seller; 
    X : person1.int 
    else 
    Buyer[R] ~> Seller; 
    X : person1.int" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Branch (                      
        (Assoc ((Location "Buyer"),
           (Condition ((INT 4), Lt, (INT 5),
              (Some BoolType))),
           None)),
        (Sync ((Location "Buyer"), (Direction "L"),
           (Location "Seller"),
           (ChoreoVars ((Name "X"),
              (Some (DotType ((Location "person1"), IntType))))),
           None)),
        (Sync ((Location "Buyer"), (Direction "R"),
           (Location "Seller"),
           (ChoreoVars ((Name "X"),
              (Some (DotType ((Location "person1"), IntType))))),
           None)),
        None)) in
      assert_equal expected expr
  | None -> assert_failure "Failed to parse expression"  

let test_parse_let_in1 _ =
  let input = "let l.(x : int) := l.5 in l.x" in
  let lexer = from_string input in
  match Parser.prog Lexer.read lexer with
  | Some expr ->
      let expected = (Let ((Location "l"),     
   (Variable ((Name "x"), (Some IntType))),
   (Assoc ((Location "l"), (INT 5), None)),
   (Assoc ((Location "l"), (Variable ((Name "x"), None)),
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
      let expected = (Let ((Location "l"),     
      (Variable ((Name "x"), (Some IntType))),
      (Assoc ((Location "l"), (INT 5), None)),
      (Assoc ((Location "l"),
         (Plus ((Variable ((Name "x"), None)), (INT 3),
            (Some IntType))),
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
      let expected = (Application (                 
        (Fun ((Name "fname"),
           (ChoreoVars ((Name "X"),
              (Some (DotType ((Location "p"), IntType))))),
           (Let ((Location "p"),
              (Variable ((Name "n"), (Some IntType))),
              (ChoreoVars ((Name "X"), None)),
              (Assoc ((Location "p"),
                 (Plus ((Variable ((Name "n"), None)),
                    (INT 1), (Some IntType))),
                 None)),
              None)),
           None)),
        (Let ((Location "p"),
           (Variable ((Name "m"), (Some IntType))),
           (Assoc ((Location "p"), (INT 3), None)),
           (Assoc ((Location "p"),
              (Minus ((Variable ((Name "m"), None)), (INT 1),
                 (Some IntType))),
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
      let expected = Let {fst = Assoc {loc = "Person2"; arg = (Variable "d")};
      snd = Snd {sndr = Assoc {loc = "Person1"; arg = (Variable "amt_due")}; name = "Person2"};
      thn = Application { funct = Fun {name = "initpay"; arg = (ChoreoVars "X");
      body = Branch { ift = Assoc {loc = "Person2"; arg = Condition {lft = (Variable "x"); op = "<"; rght = (Value 500)}};
      thn = Sync {sndr = "Person2"; d = "L"; rcvr = "Person1"; thn = Let { fst = Assoc {loc = "Person2"; arg = (Variable "rcv")};
      snd = Snd { sndr = Assoc {loc = "Person1"; arg = (Variable "rem")}; name = "Person2"};
      thn = Let { fst = Assoc {loc = "Person2"; arg = (Variable "y")};
      snd = Snd { sndr = Assoc {loc = "Person1"; arg = Minus {lft = (Variable "amt_due");  rght = (Variable "rem")}}; 
      name = "Person2"}; thn = Assoc {loc = "Person2"; arg = (Variable "d")}}}};
      el = Sync {sndr = "Person2"; d = "R"; rcvr = "Person1"; thn = Let {fst = Assoc {loc = "Person2"; arg = (Variable "rcv")};
      snd = Snd { sndr = Assoc {loc = "Person1"; arg = (Value 500)}; name = "Person2"};
      thn = Let {fst = Assoc {loc = "Person2"; arg = (Variable "y")}; 
      snd = Snd {sndr = Assoc {loc = "Person1"; arg = (Value 0)}; name = "Person2"};thn = (ChoreoVars "X")}}}}};
      argument = Assoc {loc = "Person2"; arg = (Variable "d")}}}
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
    (* "test_parse_application_local_success" >:: test_parse_application_local_success;    *)
    "test_parse_sync_success" >:: test_parse_sync_success;
    "test_parse_if_thn_else_success" >:: test_parse_if_thn_else_success;
    "test_parse_let_in1" >:: test_parse_let_in1;
    "test_parse_let_in2" >:: test_parse_let_in2;
    "test_parse_application_letin" >:: test_parse_application_letin;
    (* "test_parse_complex_success" >:: test_parse_complex_success; *)
    (* Add more test cases here *)
  ]

let () = run_test_tt_main ("parser_tests" >::: [suite]);