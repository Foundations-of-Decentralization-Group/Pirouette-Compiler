open OUnit2
open Ast_core

module DummyInfo = struct
  type t = int
end

module LocalAst = Local.With(DummyInfo)
(* let test_type_bool (new_meta: int) (old_meta: int) (input_bool: bool) =
  let old_info = Local.M.TBool(input_bool, old_meta) in
  let new_info = LocalAst.set_info_typ new_meta (old_info) in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; *)
let test_change_bool (new_meta: int) (old_meta: int) (input_bool: bool) =
  let old_info = Local.M.Bool(input_bool, old_meta) in
  let new_info = LocalAst.set_info_value new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_value (new_info))
;;
let test_change_string (new_meta: int) (old_meta: int) (input_string: string) =
    let old_info = Local.M.String(input_string, old_meta) in
    let new_info = LocalAst.set_info_value new_meta old_info in
    assert_equal new_meta (LocalAst.get_info_value (new_info))
;;

let test_change_int (old_int:'a) (new_int:'a) =
  let old_info = Local.M.Int(old_int, old_int) in
  let new_info = LocalAst.set_info_value new_int old_info in
  assert_equal new_int (LocalAst.get_info_value (new_info))
;;

(* let test_simple _ =
  assert_equal true true put a ast_core test here *)
let suite =
  "Ast_core Tests" 
  >::: ["Int Tests" 
    >::: [
    ("test_simple" >:: fun _ -> test_change_int 1 2);
    ("test_simple" >:: fun _ -> test_change_int 2 3);
   ("test_simple" >:: fun _ -> test_change_int 10 20);
    ("test_simple" >:: fun _ -> test_change_int (-1) (-2));
    ("test_simple" >:: fun _ -> test_change_int 1000 2000);
  ];"string Tests" 
  >::: [
  ("test_simple" >:: fun _ -> test_change_string 1 2 ("hello"));
  ("test_simple" >:: fun _ -> test_change_string 2 3 ("hellonjdvsbvjsbgjkbs"));
  ("test_simple" >:: fun _ -> test_change_string 10 20 ("hi"));
  ("test_simple" >:: fun _ -> test_change_string 1000 2000 ("hello"));
  ("test_simple" >:: fun _ -> test_change_string (-1) (-2) ("hello"));
];"bool Tests" 
>::: [
("test_simple" >:: fun _ -> test_change_bool 1 2 (false));
("test_simple" >:: fun _ -> test_change_bool 2 3 (true));
("test_simple" >:: fun _ -> test_change_bool 10 20 (false));
("test_simple" >:: fun _ -> test_change_bool 1000 2000 (true));
("test_simple" >:: fun _ -> test_change_bool (-1) (-2) (false));
]
  ]


let () = 
  run_test_tt_main suite
;;