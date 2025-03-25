open OUnit2
open Ppxlib
open Ast_core.Net.M
open Codegen.Emit_core
open Ast_core.Local.M

let expr_to_string expr = Pprintast.string_of_expression expr
(* let struct_to_string str = Pprintast.string_of_structure str *)
(* let loc = { !Ast_helper.default_loc with loc_ghost = true } *)

let contains_substring string substring =
  let s_length = String.length string in
  let sub_length = String.length substring in
  if sub_length = 0
  then true
  else if sub_length > s_length
  then false
  else (
    let rec check_subs i =
      if i > s_length - sub_length
      then false
      else if String.sub string i sub_length = substring
      then true
      else check_subs (i + 1)
    in
    check_subs 0)
;;

(*---------------------- Local Expression Tests ------------------------- *)

let local_unit _ =
  let expr = Unit () in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "()")
;;

let local_int _ =
  let expr = Val (Int (12, ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "12")
;;

let local_true _ =
  let expr = Val (Bool (true, ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "true")
;;

let local_false _ =
  let expr = Val (Bool (false, ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "false")
;;

let local_string _ =
  let expr = Val (String ("hello", ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "\"hello\"")
;;

let test_local_var _ =
  let expr = Var (VarId ("Alice", ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "Alice")
;;

let local_unop_not _ =
  let expr = UnOp (Not (), Val (Bool (true, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "not");
  assert_equal true (contains_substring result "true")
;;

let local_unop_neg _ =
  let expr = UnOp (Neg (), Val (Int (5, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "-");
  assert_equal true (contains_substring result "5")
;;

let local_binop_plus _ =
  let expr = BinOp (Val (Int (5, ()), ()), Plus (), Val (Int (3, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "5");
  assert_equal true (contains_substring result "+");
  assert_equal true (contains_substring result "3")
;;

let local_binop_minus _ =
  let expr = BinOp (Val (Int (10, ()), ()), Minus (), Val (Int (4, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "10");
  assert_equal true (contains_substring result "-");
  assert_equal true (contains_substring result "4")
;;

let local_binop_times _ =
  let expr = BinOp (Val (Int (5, ()), ()), Times (), Val (Int (7, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "5");
  assert_equal true (contains_substring result "*");
  assert_equal true (contains_substring result "7")
;;

let local_binop_and _ =
  let expr = BinOp (Val (Bool (true, ()), ()), And (), Val (Bool (false, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "true");
  assert_equal true (contains_substring result "&&");
  assert_equal true (contains_substring result "false")
;;

let local_binop_or _ =
  let expr = BinOp (Val (Bool (true, ()), ()), Or (), Val (Bool (false, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "true");
  assert_equal true (contains_substring result "||");
  assert_equal true (contains_substring result "false")
;;

let local_binop_eq _ =
  let expr = BinOp (Val (Int (15, ()), ()), Eq (), Val (Int (15, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "15");
  assert_equal true (contains_substring result "=");
  assert_equal true (contains_substring result "15")
;;

let local_binop_lt _ =
  let expr = BinOp (Val (Int (5, ()), ()), Lt (), Val (Int (10, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "5");
  assert_equal true (contains_substring result "<");
  assert_equal true (contains_substring result "10")
;;

let local_binop_div _ =
  let expr = BinOp (Val (Int (10, ()), ()), Div (), Val (Int (2, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "10");
  assert_equal true (contains_substring result "/");
  assert_equal true (contains_substring result "2")
;;

let local_binop_neq _ =
  let expr = BinOp (Val (Int (6, ()), ()), Neq (), Val (Int (9, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "6");
  assert_equal true (contains_substring result "<>");
  assert_equal true (contains_substring result "9")
;;

let local_binop_leq _ =
  let expr = BinOp (Val (Int (2, ()), ()), Leq (), Val (Int (12, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "2");
  assert_equal true (contains_substring result "<=");
  assert_equal true (contains_substring result "12")
;;

let local_binop_gt _ =
  let expr = BinOp (Val (Int (14, ()), ()), Gt (), Val (Int (4, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "14");
  assert_equal true (contains_substring result ">");
  assert_equal true (contains_substring result "4")
;;

let local_binop_geq _ =
  let expr = BinOp (Val (Int (15, ()), ()), Geq (), Val (Int (1, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "15");
  assert_equal true (contains_substring result ">=");
  assert_equal true (contains_substring result "1")
;;

let local_let_int _ =
  let expr =
    Let (VarId ("x", ()), TUnit (), Val (Int (2, ()), ()), Var (VarId ("x", ()), ()), ())
  in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "let rec x =");
  (*the let bindings are recursive let bindings!!*)
  assert_equal true (contains_substring result "2");
  assert_equal true (contains_substring result "in x")
;;

let local_let_nested _ =
  let expr =
    Let
      ( VarId ("x", ())
      , TUnit ()
      , Val (Int (1, ()), ())
      , Let
          ( VarId ("y", ())
          , TUnit ()
          , Val (Int (2, ()), ())
          , BinOp (Var (VarId ("x", ()), ()), Plus (), Var (VarId ("y", ()), ()), ())
          , () )
      , () )
  in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "let rec x = 1");
  assert_equal true (contains_substring result "let rec y = 2");
  assert_equal true (contains_substring result "x + y")
;;

let local_pair _ =
  let expr = Pair (Val (Int (1, ()), ()), Val (Bool (true, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "1");
  assert_equal true (contains_substring result ",");
  assert_equal true (contains_substring result "true")
;;

let local_fst _ =
  let expr = Fst (Pair (Val (Int (1, ()), ()), Val (Bool (true, ()), ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "fst");
  assert_equal true (contains_substring result "1");
  assert_equal true (contains_substring result "true")
;;

let local_snd _ =
  let expr = Snd (Pair (Val (Int (1, ()), ()), Val (Bool (true, ()), ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "snd");
  assert_equal true (contains_substring result "1");
  assert_equal true (contains_substring result "true")
;;

let local_left _ =
  let expr = Left (Val (Bool (true, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "Left");
  assert_equal true (contains_substring result "true")
;;

let local_right _ =
  let expr = Right (Val (Bool (false, ()), ()), ()) in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "Right");
  assert_equal true (contains_substring result "false")
;;

let local_match_int _ =
  let expr =
    Match
      ( Left (Val (Int (123, ()), ()), ())
      , [ Left (Var (VarId ("x", ()), ()), ()), Var (VarId ("x", ()), ())
        ; Right (Var (VarId ("y", ()), ()), ()), Val (Bool (false, ()), ())
        ]
      , () )
  in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "match");
  assert_equal true (contains_substring result "Left");
  assert_equal true (contains_substring result "Right");
  assert_equal true (contains_substring result "123");
  assert_equal true (contains_substring result "x");
  assert_equal true (contains_substring result "y");
  assert_equal true (contains_substring result "false")
;;

let local_match_pair _ =
  let expr =
    Match
      ( Pair (Val (Int (123, ()), ()), Val (Bool (true, ()), ()), ())
      , [ ( Pair (Var (VarId ("Alice", ()), ()), Var (VarId ("Bob", ()), ()), ())
          , Var (VarId ("Alice", ()), ()) )
        ]
      , () )
  in
  let result = expr_to_string (emit_local_pexp expr) in
  assert_equal true (contains_substring result "match");
  assert_equal true (contains_substring result "123");
  assert_equal true (contains_substring result "true");
  assert_equal true (contains_substring result "Alice");
  assert_equal true (contains_substring result "Bob")
;;

(*----------------------------FFI test cases--------------------------------------*)

let test_basic_external_function _ =
  let binding = emit_foreign_decl "my_func" (TUnit ()) "simple_function" in
  let result = expr_to_string binding.pvb_expr in
  Printf.printf "Expected: fun arg -> Simple_function.simple_function arg\n";
  Printf.printf "Got: %s\n" (String.trim result);
  assert_equal
    ~msg:"Basic external function should create a simple wrapper"
    "fun arg -> Simple_function.simple_function arg"
    (String.trim result)
;;

let test_module_path_external_function _ =
  let binding = emit_foreign_decl "custom_fn" (TUnit ()) "@utils/math:calculate" in
  let result = expr_to_string binding.pvb_expr in
  Printf.printf "Expected: fun arg -> Math.calculate arg\n";
  Printf.printf "Got: %s\n" (String.trim result);
  assert_equal
    ~msg:"Module path external function should create proper module access"
    "fun arg -> Math.calculate arg"
    (String.trim result)
;;

let test_nested_module_path_external_function _ =
  let binding = emit_foreign_decl "deep_fn" (TUnit ()) "@utils/math/deep:calculate" in
  let result = expr_to_string binding.pvb_expr in
  Printf.printf "Expected: fun arg -> Deep.calculate arg\n";
  Printf.printf "Got: %s\n" (String.trim result);
  assert_equal
    ~msg:"Nested module path external function should create proper module access"
    "fun arg -> Deep.calculate arg"
    (String.trim result)
;;

let test_invalid_external_format _ =
  assert_raises
    (Failure "Invalid external function format. Expected @file:function")
    (fun () -> emit_foreign_decl "bad_fn" (TUnit ()) "@invalid_format" |> ignore)
;;

let test_empty_module_path _ =
  assert_raises
    (Failure "Invalid external function format. Expected @file:function")
    (fun () -> emit_foreign_decl "bad_fn" (TUnit ()) "@:function" |> ignore)
;;

let test_empty_function_name _ =
  assert_raises
    (Failure "Invalid external function format. Expected @file:function")
    (fun () -> emit_foreign_decl "bad_fn" (TUnit ()) "@module:" |> ignore)
;;

let local_expr_suite =
  "Local expression tests"
  >::: [ "Unit" >:: local_unit
       ; "Int" >:: local_int
       ; "Bool (true)" >:: local_true
       ; "Bool (false)" >:: local_false
       ; "String" >:: local_string
       ; "Variable" >:: test_local_var
       ; "UnOp (not)" >:: local_unop_not
       ; "UnOp (neg)" >:: local_unop_neg
       ; "BinOp (plus)" >:: local_binop_plus
       ; "BinOp (minus)" >:: local_binop_minus
       ; "BinOp (times)" >:: local_binop_times
       ; "BinOp (div)" >:: local_binop_div
       ; "BinOp (and)" >:: local_binop_and
       ; "BinOp (or)" >:: local_binop_or
       ; "BinOp (eq)" >:: local_binop_eq
       ; "BinOp (neq)" >:: local_binop_neq
       ; "BinOp (lt)" >:: local_binop_lt
       ; "BinOp (leq)" >:: local_binop_leq
       ; "BinOp (gt)" >:: local_binop_gt
       ; "BinOp (geq)" >:: local_binop_geq
       ; "Let (int)" >:: local_let_int
       ; "Let (nested)" >:: local_let_nested
       ; "Pair" >:: local_pair
       ; "Fst" >:: local_fst
       ; "Snd" >:: local_snd
       ; "Left" >:: local_left
       ; "Right" >:: local_right
       ; "Match (int)" >:: local_match_int
       ; "Match (pair)" >:: local_match_pair
       ]
;;

let ffi_suite =
  "Foreign function tests"
  >::: [ "test_basic_external_function" >:: test_basic_external_function
       ; "test_module_path_external_function" >:: test_module_path_external_function
       ; "test_nested_module_path_external_function"
         >:: test_nested_module_path_external_function
       ; "test_invalid_external_format" >:: test_invalid_external_format
       ; "test_empty_module_path" >:: test_empty_module_path
       ; "test_empty_function_name" >:: test_empty_function_name
       ]
;;

let all_suites = "Emit_Core Tests" >::: [ local_expr_suite; ffi_suite ]
let () = run_test_tt_main all_suites
