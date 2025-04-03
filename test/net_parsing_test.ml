open OUnit2
open Lexing

(* Helper function to parse a string *)
let parse_string str =
  let lexbuf = Lexing.from_string str in
  (* Set the filename in the lexbuf position for better error messages *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "test_net" };
  Parsing.Net_parser.prog Parsing.Net_lexer.read lexbuf

let test_basic_declarations _ =
  let test_cases = [
    (* Basic variable declaration *)
    "x: unit_t;",
    "Simple unit type declaration";

    (* Multiple variable declaration *)
    "x, y: int_t;",
    "Multiple variable declaration";

    (* Type declaration *)
    "type MyType = unit_t;",
    "Type declaration";

    (* Foreign declaration *)
    "foreign print: string_t -> unit_t := \"console.log\";",
    "Foreign function declaration"
  ]
  in
  List.iter (fun (input, msg) ->
    (* Now assert that parse_string doesn't raise an exception *)
    try
      let _ = parse_string input in
      assert_bool msg true (* Indicate success if no exception *)
    with Failure explanation -> (* Catch the specific failure *)
      assert_failure (Printf.sprintf "%s: Failed for input '%s'. Reason: %s" msg input explanation)
  ) test_cases

let test_network_expressions _ =
  let test_cases = [
    (* Basic send/receive *)
    "x := send 42 ~> client;",
    "Send expression";

    "x := recv from server;",
    "Receive expression";

    (* Choose and Allow *)
    "x := choose Login for client in e;",
    "Choose expression";

    "x := allow choice from client with
      | Login -> ret true
      | Logout -> ret false;",
    "Allow choice expression"
  ]
  in
  List.iter (fun (input, msg) ->
    assert_bool msg (try let _ = parse_string input in true
                     with _ -> false)
  ) test_cases

let test_local_expressions _ =
  let test_cases = [
    (* Basic arithmetic *)
    "x := ret (1 + 2);",
    "Basic arithmetic";

    (* Boolean operations *)
    "x := ret (true && false);",
    "Boolean operations";

    (* Let expressions *)
    "x := let y: int_t := 42 in ret y;",
    "Let expression";

    (* Pattern matching *)
    "x := match y with
      | Left z -> ret z
      | Right w -> ret w;",
    "Pattern matching"
  ]
  in
  List.iter (fun (input, msg) ->
    assert_bool msg (try let _ = parse_string input in true
                     with _ -> false)
  ) test_cases

let test_complex_expressions _ =
  let test_cases = [
    (* Nested let expressions *)
    "x := let y: int_t := 1 in
          let z: int_t := 2 in
          ret (y + z);",
    "Nested let expressions";

    (* Function definition *)
    "f := fun x -> ret (x + 1);",
    "Function definition";

    (* Pairs and projections *)
    "x := ret (fst (1, 2));",
    "Pair projection";

    (* Complex pattern matching *)
    "x := match p with
      | (Left x, Right y) -> ret x
      | (Right x, Left y) -> ret y
      | _ -> ret 0;",
    "Complex pattern matching"
  ]
  in
  List.iter (fun (input, msg) ->
    assert_bool msg (try let _ = parse_string input in true
                     with _ -> false)
  ) test_cases

let test_error_cases _ =
  let test_cases = [
    (* Missing semicolon *)
    "x: unit_t",
    "Should fail: Missing semicolon";

    (* Invalid syntax *)
    "x :== 42;",
    "Should fail: Invalid assignment operator";

    (* Mismatched parentheses *)
    "x := (1 + 2;",
    "Should fail: Mismatched parentheses"
  ]
  in
  List.iter (fun (input, msg) ->
    assert_bool msg (try let _ = parse_string input in false
                     with _ -> true)
  ) test_cases

let suite =
  "net_parser_test" >::: [
    "test_basic_declarations" >:: test_basic_declarations;
    "test_network_expressions" >:: test_network_expressions;
    "test_local_expressions" >:: test_local_expressions;
    "test_complex_expressions" >:: test_complex_expressions;
    "test_error_cases" >:: test_error_cases;
  ]

let () = run_test_tt_main suite
