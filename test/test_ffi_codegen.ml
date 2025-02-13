open OUnit2
open Ppxlib
open Ast_core.Net.M  
open Codegen.Emit_core

let expr_to_string expr =
  Pprintast.string_of_expression expr

let test_basic_external_function _ =
  let binding = emit_foreign_decl "my_func" (TUnit ()) "simple_function" in
  let result = expr_to_string binding.pvb_expr in
  Printf.printf "Expected: fun arg -> Simple_function.simple_function arg\n";
  Printf.printf "Got: %s\n" (String.trim result);
  assert_equal 
    ~msg:"Basic external function should create a simple wrapper"
    "fun arg -> Simple_function.simple_function arg"
    (String.trim result)

let test_module_path_external_function _ =
  let binding = emit_foreign_decl "custom_fn" (TUnit ()) "@utils/math:calculate" in
  let result = expr_to_string binding.pvb_expr in
  Printf.printf "Expected: fun arg -> Math.calculate arg\n";
  Printf.printf "Got: %s\n" (String.trim result);
  assert_equal 
    ~msg:"Module path external function should create proper module access"
    "fun arg -> Math.calculate arg"
    (String.trim result)

let test_invalid_external_format _ =
  assert_raises 
    (Failure "Invalid external function format. Expected @file:function")
    (fun () -> 
       emit_foreign_decl "bad_fn" (TUnit ()) "@invalid_format" |> ignore)

let suite =
  "EmitCoreForeignTests" >::: [
    "test_basic_external_function" >:: test_basic_external_function;
    "test_module_path_external_function" >:: test_module_path_external_function;
    "test_invalid_external_format" >:: test_invalid_external_format;
  ]

let () =
  run_test_tt_main suite