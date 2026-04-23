a : A.(int+unit);

{- 'type' currently doesnt do anything -}
type bld := A.(int*unit);

a1 := A.(left 10);
a2 := A.(right ());

a3 := A.(left 500);

isunit : A.(int+unit) -> A.string;
isunit x := 
	match x with
		| left l -> (A.string_cat A."x is " (A.string_of_int l))
		| right _ -> A."x is empty"
;

main :=
  let b1 := isunit a1; in
  let _ := A.print_endline b1; in

  let A.b2 := isunit a2; in
  [A] b2 ~> B.msg;
  let _ := B.print_endline B.msg; in

  let A.b3 := isunit a3; in
  let B.msg3 := [A] b3~>B; in
  let _ := B.print_endline B.msg3; in
  A.0
  ;
