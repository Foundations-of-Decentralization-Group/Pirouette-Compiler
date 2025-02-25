open OUnit2

let test_simple _ =
  assert_equal true true (* put a ast_core test here *)
let suite =
  "Ast_core Tests" >::: [
    "test_simple" >:: test_simple;
  ]


let () = 
  run_test_tt_main suite
;;