open OUnit2
open Ast_core
open Local.M

(* open M *)

(* module DummyInfo = struct
  type t = 'a 
end *)

(* module With (Info : sig
  type t
end) =
struct
type nonrec value = Info.t M.value
type nonrec loc_id = Info.t M.loc_id
type nonrec var_id = Info.t M.var_id
type nonrec typ_id = Info.t M.typ_id
type nonrec sync_label = Info.t M.sync_label
type nonrec un_op = Info.t M.un_op
type nonrec bin_op = Info.t M.bin_op
type nonrec typ = Info.t M.typ
type nonrec pattern = Info.t M.pattern
type nonrec expr = Info.t M.expr*)

(* module LocalAst = Local.With()  *)
module DummyInfo = struct
  type t = int
end

module LocalAst = Local.With(DummyInfo)


(* let test_change_string (old_string: string) (new_string: string) =
    let old_info = Local.M.String(old_string, old_string) in
    let new_info = LocalAst.set_info_value new_string old_info in
    assert_equal new_string (LocalAst.get_info_value (new_info))
;; *)

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
  (* ];"string Tests" 
  >::: [
  ("test_simple" >:: fun _ -> test_change_string ("hi") ("hello"));
  ("test_simple" >:: fun _ -> test_change_string 2 3);
 ("test_simple" >:: fun _ -> test_change_string 10 20);
  ("test_simple" >:: fun _ -> test_change_string (-1) (-2));
  ("test_simple" >:: fun _ -> test_change_string 1000 2000); *)
]
  ]


let () = 
  run_test_tt_main suite
;;