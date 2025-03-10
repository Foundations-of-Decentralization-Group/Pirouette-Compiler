open OUnit2
open Ast_core

module DummyInfo = struct
  type t = int 
end

module LocalAst = Local.With(DummyInfo)
module ChoreoAst = Ast_core.Choreo.With(DummyInfo)

let test_pattern_default (old_meta: int) (new_meta: int)  =
  let old_info = Choreo.M.Default(1) in
  let new_info = ChoreoAst.set_info_pattern old_meta old_info in
  assert_equal old_meta (ChoreoAst.get_info_pattern (new_info));
  let new_info = ChoreoAst.set_info_pattern new_meta old_info in
  assert_equal new_meta (ChoreoAst.get_info_pattern (new_info));
;;
let test_pattern_variable (old_meta: int) (new_meta: int) =
  let var_id = Local.M.VarId("hi", 1) in
  let (val_pat: int Choreo.M.pattern) = Choreo.M.Var (var_id, old_meta) in
  let (new_info: int Choreo.M.pattern) = ChoreoAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (ChoreoAst.get_info_pattern (new_info));
;;
let test_pattern_pair (old_meta: int) (new_meta: int) =
  let var_id = Local.M.VarId ("hi",old_meta) in
  let pat_pair1 : int Choreo.M.pattern = Choreo.M.Var (var_id, old_meta) in
  let pat_pair2 : int Choreo.M.pattern = Choreo.M.Var (var_id, old_meta) in
  let (val_pat: int Choreo.M.pattern) = Choreo.M.Pair (pat_pair1, pat_pair2, old_meta) in
  let (new_info: int Choreo.M.pattern) = ChoreoAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (ChoreoAst.get_info_pattern (new_info));
;;
(* let test_pattern_right (old_meta: int) (new_meta: int) =
  let var_id = Local.M.VarId ("hi", 1) in
  let (var_pat1: int Local.M.pattern) = Local.M.Var (var_id, old_meta) in
  let (val_pat: int Local.M.pattern) = Local.M.Right (var_pat1, old_meta) in
  let (new_info: int Local.M.pattern) = LocalAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;;
let test_pattern_left (old_meta: int) (new_meta: int) =
  let var_id = Local.M.VarId ("hi", 1) in
  let (var_pat1: int Local.M.pattern) = Local.M.Var (var_id, old_meta) in
  let (val_pat: int Local.M.pattern) = Local.M.Left (var_pat1, old_meta) in
  let (new_info: int Local.M.pattern) = LocalAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;;  *)

 


let suite =
  "Ast_core Choreo Tests" 
  >::: ["Pattern Tests" 
    >::: [
    ("test_pattern_default" >:: fun _ -> test_pattern_default 1 2);
    ("test_pattern_variable" >:: fun _ -> test_pattern_variable 1 2);
    ("test_pattern_pair" >:: fun _ -> test_pattern_pair 1 2);

    (* ("test_pattern_right" >:: fun _ -> test_pattern_right 1 2);
    ("test_pattern_left" >:: fun _ -> test_pattern_left 1 2 ); *)
  ]
  ]


let () = 
run_test_tt_main suite
;;