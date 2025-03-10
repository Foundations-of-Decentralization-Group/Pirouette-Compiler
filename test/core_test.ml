open OUnit2
open Ast_core

module DummyInfo = struct
  type t = int 
end

module LocalAst = Local.With(DummyInfo)
module ChoreoAst = Ast_core.Choreo.With(DummyInfo)

let test_expression_match (old_meta: int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_pair1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let expr_pair2 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let pat_pair1 : int Local.M.pattern = Local.M.Val (var_int, old_meta) in
  let (val_pat: int Local.M.expr) = Local.M.Match (expr_pair1, [(pat_pair1, expr_pair2)], old_meta) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_expression_right (old_meta:int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let var_unit = Local.M.Right (expr_1, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta var_unit in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_expression_left (old_meta:int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let var_unit = Local.M.Left (expr_1, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta var_unit in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 

let test_expression_snd (old_meta:int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let var_unit = Local.M.Snd (expr_1, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta var_unit in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_expression_fst (old_meta:int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let var_unit = Local.M.Fst (expr_1, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta var_unit in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_expression_pair (old_meta: int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let expr_pair1 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let expr_pair2 : int Local.M.expr = Local.M.Val (var_int, old_meta) in
  let (val_pat: int Local.M.expr) = Local.M.Pair (expr_pair1, expr_pair2, old_meta) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 

let test_expression_let (old_meta:int) (new_meta: int) (input_int: int) (input_int2: int) =
  let var_id = Local.M.VarId ("hi", old_meta) in
  let old_info = Local.M.TSum ((Local.M.TUnit old_meta),(Local.M.TString input_int),input_int2) in
  let (val_int: int Local.M.expr) = Local.M.Var (var_id, old_meta) in
  let (let_expr: int Local.M.expr) = Local.M.Let (var_id , old_info, val_int, val_int, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta let_expr in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;;
let test_expression_binop (old_meta:int) (new_meta: int) =
  let var_int = Local.M.VarId ("hi", old_meta) in
  let (val_int: int Local.M.expr) = Local.M.Var (var_int, old_meta) in
  let plus = Local.M.Plus(1) in
  let (bi_op: int Local.M.expr) = Local.M.BinOp (val_int, plus, val_int, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta bi_op in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;;
let test_expression_unop (old_meta:int) (new_meta: int) =
  let var_int = Local.M.VarId ("hi", old_meta) in
  let (val_int: int Local.M.expr) = Local.M.Var (var_int, old_meta) in
  let neg = Local.M.Neg(1) in
  let (un_op: int Local.M.expr) = Local.M.UnOp (neg, val_int, 1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta un_op in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;;
let test_expression_var (old_meta:int) (new_meta: int) =
  let var_int = Local.M.VarId ("hi", old_meta) in
  let (val_int: int Local.M.expr) = Local.M.Var (var_int, old_meta) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta val_int in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;;

let test_expression_val (old_meta:int) (new_meta: int) =
  let var_int = Local.M.Int (1, old_meta) in
  let (val_int: int Local.M.expr) = Local.M.Val (var_int, old_meta) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta val_int in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_expression_unit (new_meta: int) =
  let var_unit = Local.M.Unit (1) in
  let (new_info: int Local.M.expr) = LocalAst.set_info_expr new_meta var_unit in
  assert_equal new_meta (LocalAst.get_info_expr (new_info));
;; 
let test_pattern_pair (old_meta: int) (new_meta: int) =
  let var_int = Local.M.Int (1,old_meta) in
  let pat_pair1 : int Local.M.pattern = Local.M.Val (var_int, old_meta) in
  let pat_pair2 : int Local.M.pattern = Local.M.Val (var_int, old_meta) in
  let (val_pat: int Local.M.pattern) = Local.M.Pair (pat_pair1, pat_pair2, old_meta) in
  let (new_info: int Local.M.pattern) = LocalAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;; 
let test_pattern_right (old_meta: int) (new_meta: int) =
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
;; 
let test_pattern_variable (old_meta: int) (new_meta: int) =
  let var_id = Local.M.VarId ("hi", 1) in
  let (val_pat: int Local.M.pattern) = Local.M.Var (var_id, old_meta) in
  let (new_info: int Local.M.pattern) = LocalAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;; 
let test_pattern_value (old_meta: int) (new_meta: int) =
  let val_int = Local.M.Int (1,old_meta) in
  let (val_pat: int Local.M.pattern) = Local.M.Val (val_int, 1) in
  let (new_info: int Local.M.pattern) = LocalAst.set_info_pattern new_meta val_pat in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;; 
let test_pattern_default (old_meta: int) (new_meta: int)  =
  let old_info = Local.M.Default(1) in
  let new_info = LocalAst.set_info_pattern old_meta old_info in
  assert_equal old_meta (LocalAst.get_info_pattern (new_info));
  let new_info = LocalAst.set_info_pattern new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_pattern (new_info));
;; 
let test_loc_id (old_meta: int) (new_meta: int)  =
  let old_info = Local.M.LocId("string", old_meta) in
  let new_info = LocalAst.set_info_locid new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_locid (new_info))
;; 
let test_var_id (old_meta: int) (new_meta: int) =
  let old_info = Local.M.VarId("string", old_meta) in
  let new_info = LocalAst.set_info_varid new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_varid (new_info))
;; 
let test_type_id (old_meta: int) (new_meta: int) =
  let old_info = Local.M.TypId("string", old_meta) in
  let new_info = LocalAst.set_info_typid new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typid (new_info))
;; 
let test_type_sum (old_meta: int) (new_meta: int) (input_int: int) (input_int2: int) =
  let old_info = Local.M.TSum ((Local.M.TUnit old_meta),(Local.M.TString input_int),input_int2) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 
let test_type_prod (old_meta: int) (new_meta: int) (input_int: int) (input_int2: int) =
  let old_info = Local.M.TProd ((Local.M.TUnit old_meta),(Local.M.TString input_int),input_int2) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 

let test_type_var (old_meta: int) (new_meta: int) (input_int: int) =
  let typ_id = Local.M.TypId("string", old_meta) in
  let old_info = Local.M.TVar (typ_id ,input_int) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 
let test_type_unit (new_meta: int) (input_int: int) =
  let old_info = Local.M.TUnit(input_int) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 
let test_type_bool (new_meta: int) (input_int: int) =
  let old_info = Local.M.TBool(input_int) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 

let test_type_string (new_meta: int) (input_int: int) =
  let old_info = Local.M.TString(input_int) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;; 

let test_type_int (new_meta: int) (input_int: int) =
  let old_info = Local.M.TInt(input_int) in
  let new_info = LocalAst.set_info_typ new_meta old_info in
  assert_equal new_meta (LocalAst.get_info_typ (new_info))
;;
let test_not (new_meta: int) (old_meta: int) =
  let old_not_op = Local.M.Not(old_meta) in
  assert_equal old_meta (LocalAst.get_info_unop old_not_op);
  let new_not_op = LocalAst.set_info_unop new_meta old_not_op in
  assert_equal new_meta (LocalAst.get_info_unop new_not_op);
;;
let test_neg (new_meta: int) (old_meta: int) =
  let old_neg_op = Local.M.Neg(old_meta) in
  assert_equal old_meta (LocalAst.get_info_unop old_neg_op);
  let new_neg_op = LocalAst.set_info_unop new_meta old_neg_op in
  assert_equal new_meta (LocalAst.get_info_unop new_neg_op);
;;

let test_geq (new_meta: int) (old_meta: int) =
  let old_geq_op = Local.M.Geq(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_geq_op);
  let new_geq_op = LocalAst.set_info_binop new_meta old_geq_op in
  assert_equal new_meta (LocalAst.get_info_binop new_geq_op);
;;
let test_gt (new_meta: int) (old_meta: int) =
  let old_gt_op = Local.M.Gt(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_gt_op);
  let new_gt_op = LocalAst.set_info_binop new_meta old_gt_op in
  assert_equal new_meta (LocalAst.get_info_binop new_gt_op);
;;
let test_lt (new_meta: int) (old_meta: int) =
  let old_lt_op = Local.M.Lt(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_lt_op);
  let new_lt_op = LocalAst.set_info_binop new_meta old_lt_op in
  assert_equal new_meta (LocalAst.get_info_binop new_lt_op);
;;

let test_neq (new_meta: int) (old_meta: int) =
  let old_neq_op = Local.M.Neq(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_neq_op);
  let new_neq_op = LocalAst.set_info_binop new_meta old_neq_op in
  assert_equal new_meta (LocalAst.get_info_binop new_neq_op);
;;

let test_leq (new_meta: int) (old_meta: int) =
  let old_leq_op = Local.M.Leq(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_leq_op);
  let new_leq_op = LocalAst.set_info_binop new_meta old_leq_op in
  assert_equal new_meta (LocalAst.get_info_binop new_leq_op);
;;
let test_eq (new_meta: int) (old_meta: int) =
  let old_eq_op = Local.M.Eq(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_eq_op);
  let new_eq_op = LocalAst.set_info_binop new_meta old_eq_op in
  assert_equal new_meta (LocalAst.get_info_binop new_eq_op);
;;
let test_div (new_meta: int) (old_meta: int) =
  let old_div_op = Local.M.Div(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_div_op);
  let new_div_op = LocalAst.set_info_binop new_meta old_div_op in
  assert_equal new_meta (LocalAst.get_info_binop new_div_op);
;;

let test_times (new_meta: int) (old_meta: int) =
  let old_times_op = Local.M.Times(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_times_op);
  let new_times_op = LocalAst.set_info_binop new_meta old_times_op in
  assert_equal new_meta (LocalAst.get_info_binop new_times_op);
;;
let test_and (new_meta: int) (old_meta: int) =
  let old_and_op = Local.M.And(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_and_op);
  let new_and_op = LocalAst.set_info_binop new_meta old_and_op in
  assert_equal new_meta (LocalAst.get_info_binop new_and_op);
;;
let test_or (new_meta: int) (old_meta: int) =
  let old_or_op = Local.M.Or(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_or_op);
  let new_or_op = LocalAst.set_info_binop new_meta old_or_op in
  assert_equal new_meta (LocalAst.get_info_binop new_or_op);
;;
let test_minus (new_meta: int) (old_meta: int) =
  let old_minus_op = Local.M.Minus(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_minus_op);
  let new_minus_op = LocalAst.set_info_binop new_meta old_minus_op in
  assert_equal new_meta (LocalAst.get_info_binop new_minus_op);
;;
let test_plus (new_meta: int) (old_meta: int) =
  let old_plus_op = Local.M.Plus(old_meta) in
  assert_equal old_meta (LocalAst.get_info_binop old_plus_op);
  let new_plus_op = LocalAst.set_info_binop new_meta old_plus_op in
  assert_equal new_meta (LocalAst.get_info_binop new_plus_op);
;;

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
];"operators Tests" 
>::: [
("test_simple" >:: fun _ -> test_plus 1 2);
("test_simple" >:: fun _ -> test_minus 1 2);
("test_simple" >:: fun _ -> test_or 1 2);
("test_simple" >:: fun _ -> test_and 1 2);
("test_simple" >:: fun _ -> test_not 1 2);
("test_simple" >:: fun _ -> test_neg 1 2);
("test_simple" >:: fun _ -> test_type_int 1 2);
("test_simple" >:: fun _ -> test_type_string 1 2);
("test_simple" >:: fun _ -> test_type_bool 1 2);
("test_simple" >:: fun _ -> test_times 1 2);
("test_simple" >:: fun _ -> test_div 1 2);
("test_simple" >:: fun _ -> test_lt 1 2);
("test_simple" >:: fun _ -> test_gt 1 2);
("test_simple" >:: fun _ -> test_leq 1 2);
("test_simple" >:: fun _ -> test_geq 1 2);
("test_simple" >:: fun _ -> test_neq 1 2);
("test_simple" >:: fun _ -> test_eq 1 2);
];"type Tests" 
>::: [
("test_simple" >:: fun _ -> test_type_int 1 2);
("test_simple" >:: fun _ -> test_type_string 1 2);
("test_simple" >:: fun _ -> test_type_bool 1 2);
("test_simple" >:: fun _ -> test_type_prod 1 2 3 4);
("test_simple" >:: fun _ -> test_type_sum 1 2 3 4);
("test_simple" >:: fun _ -> test_type_var 1 2 3);
("test_simple" >:: fun _ -> test_type_unit 1 2 );
("test_simple" >:: fun _ -> test_type_id 1 2 );
("test_simple" >:: fun _ -> test_var_id 1 2 );
("test_simple" >:: fun _ -> test_loc_id 1 2 );
];"Pattern Tests" 
    >::: [
    ("test_simple" >:: fun _ -> test_pattern_default 1 2);
    ("test_simple" >:: fun _ -> test_pattern_value 1 2);
    ("test_simple" >:: fun _ -> test_pattern_variable 1 2);
    ("test_simple" >:: fun _ -> test_pattern_left 1 2);
    ("test_simple" >:: fun _ -> test_pattern_right 1 2);
    ("test_simple" >:: fun _ -> test_pattern_pair 1 2)
  ];"Expression Tests" 
  >::: [
  ("test_simple" >:: fun _ -> test_expression_unit 2);
  ("test_simple" >:: fun _ -> test_expression_val 1 2);
  ("test_simple" >:: fun _ -> test_expression_unop 1 2);
  ("test_simple" >:: fun _ -> test_expression_var 1 2);
  ("test_simple" >:: fun _ -> test_expression_binop 1 2);
  ("test_simple" >:: fun _ -> test_expression_let 1 2 3 4);
  ("test_simple" >:: fun _ -> test_expression_fst 1 2);
  ("test_simple" >:: fun _ -> test_expression_snd 1 2);
  ("test_simple" >:: fun _ -> test_expression_left 1 2);
  ("test_simple" >:: fun _ -> test_expression_right 1 2);
  ("test_simple" >:: fun _ -> test_expression_match 1 2);
  ("test_simple" >:: fun _ -> test_expression_pair 1 2);
]
  ]


let () = 
  run_test_tt_main suite
;;